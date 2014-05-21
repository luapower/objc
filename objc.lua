--objecive-c runtime binding (Cosmin Apreutesei, public domain).
--ideas and code from TLC by Fjölnir Ásgeirsson (c) 2012, MIT license.
--tested with with LuaJIT 2.0.3, 32bit and 64bit on OSX 10.9.

local ffi = require'ffi'
local bit = require'bit'

if ffi.arch ~= 'arm' and ffi.os == 'OSX' then
	ffi.load('libobjc.A.dylib', true)
end

if ffi.abi'64bit' then
	ffi.cdef[[
	typedef double CGFloat;
	typedef long NSInteger;
	typedef unsigned long NSUInteger;
	]]
else
	ffi.cdef[[
	typedef float CGFloat;
	typedef int NSInteger;
	typedef unsigned int NSUInteger;
	]]
end

ffi.cdef[[
typedef signed char BOOL;

typedef struct objc_class    *Class;
typedef struct objc_object   *id;
typedef struct objc_selector *SEL;
typedef struct objc_method   *Method;
typedef id                   (*IMP) (id, SEL, ...);
typedef struct Protocol      Protocol;
typedef struct objc_property *objc_property_t;
typedef struct objc_ivar     *Ivar;

struct objc_class  { Class isa; };
struct objc_object { Class isa; };

struct objc_method_description {
	SEL name;
	char *types;
};

//stdlib
int access(const char *path, int amode);    // used to check if a file exists
void free (void*);                          // used for freeing returned dyn. allocated objects

//selectors
SEL sel_registerName(const char *str);
const char* sel_getName(SEL aSelector);

//classes
Class objc_getClass(const char *name);
const char *class_getName(Class cls);
Class class_getSuperclass(Class cls);
Class object_getClass(id object); //for getting the metaclass
Class objc_allocateClassPair(Class superclass, const char *name, size_t extraBytes);
void objc_registerClassPair(Class cls);

//methods
Method class_getInstanceMethod(Class aClass, SEL aSelector);
SEL method_getName(Method method);
const char *method_getTypeEncoding(Method method);
IMP method_getImplementation(Method method);
IMP class_replaceMethod(Class cls, SEL name, IMP imp, const char *types);

//protocols
Protocol *objc_getProtocol(const char *name);
const char *protocol_getName(Protocol *p);
struct objc_method_description protocol_getMethodDescription(Protocol *p,
	SEL aSel, BOOL isRequiredMethod, BOOL isInstanceMethod);
BOOL class_conformsToProtocol(Class cls, Protocol *protocol);
BOOL class_addProtocol(Class cls, Protocol *protocol);

//properties
objc_property_t class_getProperty(Class cls, const char *name);
objc_property_t protocol_getProperty(Protocol *proto, const char *name,
	BOOL isRequiredProperty, BOOL isInstanceProperty);
const char *property_getName(objc_property_t property);
const char *property_getAttributes(objc_property_t property);

//ivars
Ivar object_getInstanceVariable(id obj, const char *name, void **outValue);
BOOL class_addIvar(Class cls, const char *name, size_t size, uint8_t alignment, const char *types);
const char *ivar_getTypeEncoding(Ivar ivar);
ptrdiff_t ivar_getOffset(Ivar ivar);

//inspection
Class *objc_copyClassList(unsigned int *outCount);
Protocol **objc_copyProtocolList(unsigned int *outCount);
Method *class_copyMethodList(Class cls, unsigned int *outCount);
struct objc_method_description *protocol_copyMethodDescriptionList(Protocol *p,
	BOOL isRequiredMethod, BOOL isInstanceMethod, unsigned int *outCount);
objc_property_t *class_copyPropertyList(Class cls, unsigned int *outCount);
objc_property_t *protocol_copyPropertyList(Protocol *proto, unsigned int *outCount);
Protocol **class_copyProtocolList(Class cls, unsigned int *outCount);
Protocol **protocol_copyProtocolList(Protocol *proto, unsigned int *outCount);

//messages
id objc_msgSend(id theReceiver, SEL theSelector, ...);
]]

local C = ffi.C                               --C namespace
local P = setmetatable({}, {__index = _G})    --private namespace
local objc = {}                               --public namespace
setfenv(1, P)

--helpers ----------------------------------------------------------------------------------------------------------------

local _ = string.format
local id_ctype = ffi.typeof'id'

local function ptr(p) --convert NULL pointer to nil for easier handling (say 'not ptr' instead of 'ptr == nil')
	if p == nil then return nil end
	return p
end

local function nptr(p) --convert pointer to lua number for using as table key
	if p == nil then return nil end
	local np = ffi.cast('intptr_t', p)
	local n = tonumber(np)
	assert(ffi.cast('intptr_t', n) == np) --check that we don't get pointers in the upper 13 bits on 64bit
	return n
end

local function own(p) --own a malloc()'ed pointer
	return p ~= nil and ffi.gc(p, C.free) or nil
end

local function memoize(func, cache) --special memoize that works with pointer arguments too
	cache = cache or {}
	return function(input)
		local key = input
		if type(key) == 'cdata' then
			key = nptr(key)
		end
		assert(key ~= nil, 'nil object')
		local ret = rawget(cache, key)
		if ret == nil then
			ret = func(input)
			if ret == nil then return end
			rawset(cache, key, ret)
		end
		return ret
	end
end

local function memoize2(func, cache1) --memoize a two-arg. function (:
	local memoized = memoize(function(arg1)
		return memoize(function(arg2) --each unique arg1 gets 2 closures + 1 table of overhead
			return func(arg1, arg2)
		end)
	end, cache1)
	return function(arg1, arg2)
		return memoized(arg1)(arg2)
	end
end

local function canread(path) --check that a file is readable without having to open it
	return C.access(path, bit.lshift(1,2)) == 0
end

local function citer(a) --return an iterator for a null-terminated C array
	local i = -1
	return function()
		if a == nil then return end
		i = i + 1
		if a[i] == nil then return nil end
		return a[i]
	end
end

--debugging --------------------------------------------------------------------------------------------------------------

errors = true    --log non-fatal errors to stderr
errcount = {}    --error counts per topic
logtopics = {}   --topics to log (none by default)

local function writelog(topic, fmt, ...)
	io.stderr:write(_('[objc] %-16s %s\n', topic, _(fmt, ...)))
end

local function log(topic, ...)
	if logtopics[topic] then
		writelog(topic, ...)
	end
end

local function err(topic, ...)
	errcount[topic] = (errcount[topic] or 0) + 1
	if errors then
		writelog(topic, ...)
	end
end

local function check(ok, fmt, ...) --assert with formatted strings
	if ok then return ok end
	error(_(fmt or 'assertion failed!', ...))
end

--ffi declarations -------------------------------------------------------------------------------------------------------

checkredef = false --check incompatible redefinition attempts (makes parsing slower)
printdecl = false --print C declarations to stdout (then you can grab them and make static ffi headers)
cnames = {global = {0}, struct = {0}} --C namespaces; ns[1] holds the count

local function defined(name, namespace) --check if a name is already defined in a C namespace
	return not checkredef and cnames[namespace][name]
end

local function redefined(name, namespace, new_cdecl) --check cdecl redefinitions and report on incompatible ones
	local old_cdecl = cnames[namespace][name]
	if not old_cdecl then return end
	if not checkredef then return end
	if old_cdecl == new_cdecl then return true end --already defined but same def.
	err('redefinition', '%s\nold:\n\t%s\nnew:\n\t%s', name, old_cdecl, new_cdecl)
	return true
end

local function declare(name, namespace, cdecl) --define a C type, const or function via ffi.cdef
	if redefined(name, namespace, cdecl) then return end
	local ok, cdeferr = pcall(ffi.cdef, cdecl)
	if ok then
		cnames[namespace][1] = cnames[namespace][1] + 1
		if printdecl then
			print(cdecl)
		end
	else
		if cdeferr == 'table overflow' then --fatal error from luajit: no more space for ctypes
			error'too many ctypes'
		end
		err('cdef', '%s\n\t%s', cdeferr, cdecl)
	end
	cnames[namespace][name] = checkredef and cdecl or true --only store the cdecl if needed
	return ok
end

--type parsing/conversion to C types -------------------------------------------------------------------------------------

local function optname(name) --format an optional name: if not nil, return it with a space in front
	return name and ' '..name or ''
end

local type_ctype --fw. decl

local function array_ctype(s, name, ...) --('[Ntype]', 'name') -> ctype('type', 'name[N]')
	local n,s = s:match'^%[(%d+)(.-)%]$'
	--protect pointers to arrays by enclosing the name, because `[]` has precedence over `*` in C declarations.
	--so for instance '^[8]' results in 'int (*)[8]` instead of `int *[8]`.
	if name and name:sub(1,1) == '*' then
		name = _('(%s)', name)
	end
	name = _('%s[%d]', name or '', n)
	return type_ctype(s, name, ...)
end

--note: `tag` means the struct tag in the C struct namespace; `name` means the typedef name in the C global namespace.
--for named structs only 'struct tag' is returned; for anonymous funcs the full 'struct {fields...}' is returned.
--before returning, named structs are recursively cdef'ed (unless deftype ~= 'cdef' which skips this step).
local function struct_ctype(s, name, deftype) --('{CGPoint="x"d"y"d}', 'NSPoint') -> 'struct CGPoint NSPoint'

	--break the struct/union def. in its constituent parts: keyword, tag, fields
	local kw, tag, fields = s:match'^(.)([^=]*)=?(.*).$' -- '{name=fields}'
	kw = kw == '{' and 'struct' or 'union'
	if tag == '?' or tag == '' then tag = nil end -- ? or empty means anonymous struct
	if fields == '' then fields = nil end -- empty definition means opaque struct

	if not fields and not tag then return 'void'..optname(name) end --rare case: '{?}' coming from '^{?}'

	if not fields or deftype ~= 'cdef' then --opaque named struct, or asked by caller not to be cdef'ed
		if not tag then
			err('parse', 'anonymous struct not valid here: %s', s)
			return 'void'..optname(name)
		end
		return _('%s %s%s', kw, tag, optname(name))
	end

	if not tag or not defined(tag, 'struct') then --anonymous or not alreay defined: parse it

		--parse the fields which come as '"name1"type1"name2"type2...'
		local t = {}
		local function addfield(name, s)
			if name == '' then name = nil end --empty field name means unnamed struct (different from anonymous)
			table.insert(t, type_ctype(s, name, 'cdef')) --eg. 'struct _NSPoint origin'
			return '' --remove the match
		end
		local s = fields
		local n
		while s ~= '' do
			s,n = s:gsub('^"([^"]*)"([%^]*%b{})', addfield) --try "field"{...}
			if n == 0 then
				s,n = s:gsub('^"([^"]*)"([%^]*%b())', addfield) --try "field"(...)
			end
			if n == 0 then
				s,n = s:gsub('^"([^"]+)"([%^]*%b[])', addfield) --try "field"[...]
			end
			if n == 0 then
				s,n = s:gsub('^"([^"]*)"([^"]+)', addfield) --try "field"...
			end
			if n == 0 then
				s,n = s:gsub('^"([^"]+)"$', '') --try "field"@"NSImage" from HIToolbox (possibly a bug in the xml)
			end
			assert(n > 0, s)
		end
		local ctype = _('%s%s {\n\t%s;\n}', kw, optname(tag), table.concat(t, ';\n\t'))

		--anonymous struct: return the full definition
		if not tag then
			return ctype .. optname(name)
		end

		--named struct: cdef it.
		--note: duplicate struct cdefs are rejected by luajit 2.0 with an error.
		declare(tag, 'struct', ctype .. ';')
	end

	return _('%s %s%s', kw, tag, optname(name))
end

local function bitfield_ctype(s, name) --('bN', 'name') -> 'unsigned name: N' (note: 64bit bitfields not supported)
	local n = s:match'^b(%d+)$'
	return _('unsigned %s: %d', name or '_', n)
end

local function pointer_ctype(s, name, deftype) --('^type', 'name') -> ctype('type', '*name')
	return type_ctype(s:sub(2), '*'..(name or ''), deftype)
end

local function char_ptr_ctype(s, name, ...) --('*', 'name') -> 'char *name'
	return pointer_ctype('^c', name, ...)
end

local function primitive_ctype(ctype)
	return function(s, name)
		return ctype .. optname(name)
	end
end

local ctype_decoders = {
	['c'] = primitive_ctype'char',
	['i'] = primitive_ctype'int',
	['s'] = primitive_ctype'short',
	['l'] = primitive_ctype'long', --treated as a 32-bit quantity on 64-bit programs
	['q'] = primitive_ctype'long long',

	['C'] = primitive_ctype'unsigned char',
	['I'] = primitive_ctype'unsigned int',
	['S'] = primitive_ctype'unsigned short',
	['L'] = primitive_ctype'unsigned long',
	['Q'] = primitive_ctype'unsigned long long',

	['f'] = primitive_ctype'float',
	['d'] = primitive_ctype'double',

	['B'] = primitive_ctype'BOOL',
	['v'] = primitive_ctype'void',
	['?'] = primitive_ctype'void', --unknown type; used for function pointers among other things

	['@'] = primitive_ctype'id',
	['#'] = primitive_ctype'Class',
	[':'] = primitive_ctype'SEL',

	['['] = array_ctype,    -- [Ntype]         ; N = number of elements
	['{'] = struct_ctype,   -- {name=fields}   ; struct
	['('] = struct_ctype,   -- (name=fields)   ; union
	['b'] = bitfield_ctype, -- bN              ; N = number of bits
	['^'] = pointer_ctype,  -- ^type           ; pointer
	['*'] = char_ptr_ctype, -- *               ; char* pointer
}

--convert an @encoded type encoding to a C type (as string)
--3rd arg = 'cdef' means that named structs contain field names and thus can and should be cdef'ed before returning.
function type_ctype(s, name, ...)
	local decoder = assert(ctype_decoders[s:sub(1,1)], s)
	return decoder(s, name, ...)
end

--convert a method type encoding to a C function type (as string)
local function method_ctype_string(s) --eg. 'v12@0:4c8' (retval offset arg1 offset arg2 offset ...)
	local ret_ctype
	local arg_ctypes = {}
	local function addarg(s)
		local ctype = type_ctype(s)
		if not ret_ctype then
			ret_ctype = ctype
		else
			table.insert(arg_ctypes, ctype)
		end
		return '' --remove the match
	end
	local n
	while s ~= '' do
		s,n = s:gsub('^[rnNoORV]?([%^]*%b{})%d+', addarg) --try {...}offset
		if n == 0 then
			s,n = s:gsub('[rnNoORV]?^([%^]*%b())%d+', addarg) --try (...)offset
		end
		if n == 0 then
			s,n = s:gsub('^[rnNoORV]?([%^]*%b[])%d+', addarg) --try [...]offset
		end
		if n == 0 then
			s,n = s:gsub('^[rnNoORV]?([^%d]+)%d+', addarg) --try ...offset
		end
		assert(n > 0, s)
	end
	local func_ctype = _('%s (*) (%s)', ret_ctype, table.concat(arg_ctypes, ', '))
	log('method_ctype', '%-20s %s', s, func_ctype)
	return func_ctype
end

local method_ctype = memoize(function(s) --caching to prevent ctype duplicates and avoid reparsing
	return ffi.typeof(method_ctype_string(s))
end)

--bridgesupport file parsing ---------------------------------------------------------------------------------------------

lazyfuncs = true --cdef functions on the first call rather than at the time of parsing the xml (see below)
loaddeps = false --load dependencies specified in the bridgesupport file (usually too many to be useful)

rename = {string = {}, enum = {}, typedef = {}, const = {}, ['function'] = {}} --rename table to solve name clashing

rename.typedef.mach_timebase_info = 'mach_timebase_info_t'
rename.const.TkFont = 'const_TkFont'

local function global(name, kind) --use a rename table to do renaming of certain problematic globals
	return rename[kind][name] or name
end

local xml    = {} --{expat_callback = handler}
local tag    = {} --{tag = start_tag_handler}
local endtag = {} --{tag = end_tag_handler}

local load_framework --fw. decl.

local _loaddeps
function tag.depends_on(attrs)
	if not _loaddeps then return end
	local ok, loaderr = pcall(load_framework, attrs.path)
	if not ok then
		err('load', '%s', loaderr)
	end
end

local typekey = ffi.abi'64bit' and 'type64' or 'type'
local valkey = ffi.abi'64bit' and 'value64' or 'value'

function tag.string_constant(attrs)
	rawset(objc, global(attrs.name, 'string'), attrs.value) --TODO: wrap NSStrings
end

function tag.enum(attrs)
	if attrs.ignore == 'true' then return end
	local s = attrs[valkey] or attrs.value
	if not s then return end --value not available on this platform
	rawset(objc, global(attrs.name, 'enum'), tonumber(s))
end

local function cdef_node(attrs, typedecl, deftype)
	local name = global(attrs.name, typedecl)
	--note: duplicate typedef and const defs are ignored by luajit 2.0 and don't overflow its ctype table,
	--but this is an implementation detail that we shouldn't rely on, so we guard against redefinitions.
	if defined(name, 'global') then return end
	local s = attrs[typekey] or attrs.type
	if not s then return end --type not available on this platform
	local ctype = type_ctype(s, name, deftype)
	declare(name, 'global', _('%s %s;', typedecl, ctype))
end

function tag.constant(attrs)
	cdef_node(attrs, 'const')
end

function tag.struct(attrs)
	cdef_node(attrs, 'typedef', attrs.opaque ~= 'true' and 'cdef' or nil)
end

function tag.cftype(attrs)
	cdef_node(attrs, 'typedef', 'cdef')
end

function tag.opaque(attrs)
	cdef_node(attrs, 'typedef')
end

local ftag --state for accumulating args on the current 'function' tag

tag['function'] = function(attrs)
	local name = global(attrs.name, 'function')
	--note: duplicate function defs are ignored by luajit 2.0 but they do overflow its ctype table,
	--so it's necessary that we guard against redefinitions.
	if defined(name, 'global') then return end
	ftag = {name = name, retval = type_ctype'v', args = {},
				variadic = attrs.variadic == 'true', arg_depth = 0}
end

function tag.retval(attrs)
	if not ftag then return end --canceled by prev. tag.arg
	ftag.arg_depth = ftag.arg_depth + 1
	if ftag.arg_depth > 1 then return end --skip child 'retval' types describing function pointers
	local s = attrs[typekey] or attrs.type
	if not s then ftag = nil; return end --arg not available on 32bit, cancel the recording
	ftag.retval = type_ctype(s, nil, 'retval')
end

function tag.arg(attrs)
	if not ftag then return end --canceled by prev. tag.arg or tag.retval
	ftag.arg_depth = ftag.arg_depth + 1
	if ftag.arg_depth > 1 then return end --skip child 'arg' types which describe function pointers
	local s = attrs[typekey] or attrs.type
	if s == '@?' then s = '@' end --block definition
	if not s then ftag = nil; return end --arg not available on 32bit, skip the entire function
	table.insert(ftag.args, type_ctype(s))
end

local function dec_arg_depth(attrs)
	if not ftag then return end
	ftag.arg_depth = ftag.arg_depth - 1
end

endtag.arg = dec_arg_depth
endtag.retval = dec_arg_depth

endtag['function'] = function()
	if not ftag then return end --canceled by prev. tag.arg or tag.retval
	local args = table.concat(ftag.args, ', ')
	local vararg = ftag.variadic and (#ftag.args > 0 and ', ...' or '...') or ''
	local cdecl = _('%s %s (%s%s);', ftag.retval, ftag.name, args, vararg)
	local name = ftag.name
	ftag = nil
	if lazyfuncs then
		if redefined(name, 'global', cdecl) then return end
		--delay cdef'ing the function until the first call, to avoid polluting the C namespace with unused declarations.
		--this is because in luajit2 can only hold as many as 64k ctypes total.
		rawset(objc, name, function(...)
			declare(name, 'global', cdecl)
			local func = C[name]
			rawset(objc, name, nil) --remove this wrapper; later calls will go to the C namespace directly.
			return func(...)
		end)
	else
		declare(name, 'global', cdecl)
	end
end

local function getsym(name) return C[name] end

function tag.function_alias(attrs) --these tags always come after the 'function' tags
	local name = attrs.name
	local original = attrs.original
	if lazyfuncs then
		local ok, func = pcall(getsym, original)
		if ok then
			rawset(objc, name, fctype)
		else
			err('alias', 'symbol not found %s for %s', original, name)
		end
	else
		--delay getting a cdef to the original function until the first call to the alias
		rawset(objc, name, function(...)
			local func = C[original]
			rawset(objc, name, func) --replace this wrapper with the original function
			return func(...)
		end)
	end
end

function xml.start_tag(name, attrs)
	if tag[name] then tag[name](attrs) end
end

function xml.end_tag(name)
	if endtag[name] then endtag[name]() end
end

function load_bridgesupport(path, loaddeps_thistime)
	if loaddeps_thistime ~= nil then
		_loaddeps = loaddeps_thistime
	else
		_loaddeps = loaddeps
	end
	local expat = require'expat' --runtime dependency: not needed if bridgesupport is not used
	expat.parse({path = path}, xml)
end

--loading frameworks -----------------------------------------------------------------------------------------------------

loadtypes = true --load bridgesupport files

searchpaths = {
	'/System/Library/Frameworks',
	'/Library/Frameworks',
	'~/Library/Frameworks',
}

function find_framework(name) --given a framework name or its full path, return its full path and its name
	if name:find'/' then
		-- try 'path/foo.framework'
		local path = name
		local name = path:match'([^/]+)%.framework$'
		if not name then
			-- try 'path/foo.framework/foo'
			name = path:match'([^/]+)$'
			path = name and path:sub(1, -#name-2)
		end
		if name and canread(path) then
			return path, name
		end
	else
		for i,path in pairs(searchpaths) do
			path = _('%s/%s.framework', path, name)
			if canread(path) then
				return path, name
			end
		end
	end
end

loaded = {} --{framework_name = true}
loaded_bs = {} --{framework_name = true}

function load_framework(namepath, option) --load a framework given its name or full path
	local basepath, name = find_framework(namepath)
	check(basepath, 'framework not found %s', namepath)
	if not loaded[basepath] then
		--load the framework binary which contains classes and functions
		if canread(path) then
			local path = _('%s/%s', basepath, name)
			ffi.load(path, true)
		end
		--load the bridgesupport dylib which contains callable versions of inline functions (NSMakePoint, etc.)
		local path = _('%s/Resources/BridgeSupport/%s.dylib', basepath, name)
		if canread(path) then
			ffi.load(path, true)
		end
		log('load', '%s', basepath)
		loaded[basepath] = true
	end
	if loadtypes and option ~= 'notypes' and not loaded_bs[basepath] then
		--load the bridgesupport xml file which contains typedefs and constants which we can't get from the runtime.
		local path = _('%s/Resources/BridgeSupport/%s.bridgesupport', basepath, name)
		if canread(path) then
			load_bridgesupport(path, option == 'nodeps' and false)
		end
		loaded_bs[basepath] = true
	end
end

--objective-c runtime ----------------------------------------------------------------------------------------------------

--selectors

local selector_object = memoize(function(name) --caching to prevent string creation each method call (worth it?)
	--replace '_' with ':' except at the beginning
	name = name:match('^_*') .. name:gsub('^_*', ''):gsub('_', ':')
	return ptr(C.sel_registerName(name))
end)

local function selector(name)
	if type(name) ~= 'string' then return name end
	return selector_object(name)
end

local function selector_name(sel)
    return ffi.string(ffi.C.sel_getName(sel))
end

--protocols

local function protocols() --list all loaded protocols
	return citer(own(C.objc_copyProtocolList(nil)))
end

local function protocol(name) --protocol by name
	if type(name) ~= 'string' then return name end
	return check(ptr(C.objc_getProtocol(name)), 'unknown protocol %s', name)
end

local function protocol_name(proto)
	return ffi.string(C.protocol_getName(proto))
end

local function protocol_protocols(proto) --protocols of superprotocols not included
	return citer(own(C.protocol_copyProtocolList(proto, nil)))
end

local function protocol_properties(proto) --inherited properties not included
	return citer(own(C.protocol_copyPropertyList(proto, nil)))
end

local function protocol_property(proto, name, required, readonly) --looks in superprotocols too
	return ptr(C.protocol_getProperty(proto, name, required, readonly))
end

local function protocol_methods(proto, inst, required) --inherited methods not included
	local desc = own(C.protocol_copyMethodDescriptionList(proto, required, inst, nil))
	local i = -1
	return function()
		i = i + 1
		if desc[i].name == nil then return end
		return desc[i].name, ffi.string(desc.types)
	end
end

local function protocol_method_type(proto, sel, inst, required) --looks in superprotocols too
	local desc = C.protocol_getMethodDescription(proto, sel, required, inst)
	if desc.name == nil then return end
	return ffi.string(desc.types)
end

--call f(proto, ...) for proto and all superprotocols recursively. stops when f() returns something.
local function protocol_search(proto, f, ...)
	local ret = f(proto, ...)
	if ret ~= nil then return ret end
	for proto in protocol_protocols(proto) do
		local ret = protocol_search(f, ...)
		if ret ~= nil then return ret end
	end
end

--properties

local function property_name(prop)
	return ffi.string(C.property_getName(prop))
end

local prop_attr_decoders = { --TODO: copy, retain, nonatomic, dynamic, weak, gc.
	T = function(s, t) t.type = s end,
	V = function(s, t) t.ivar = s end,
	G = function(s, t) t.getter = s end,
	S = function(s, t) t.setter = s end,
	R = function(s, t) t.readonly = true end,
}
local property_attrs = memoize(function(prop) --caching to prevent parsing on each property access
	local s = ffi.string(C.property_getAttributes(prop))
	local attrs = {}
	for k,v in (s..','):gmatch'(.)([^,]*),' do
		local decode = prop_attr_decoders[k]
		if decode then decode(v, attrs) end
	end
	return attrs
end)

local function property_getter(prop)
	local attrs = property_atts(prop)
	if not attrs.getter then
		attrs.getter = property_name(prop) --default getter; cache it
	end
	return attrs.getter
end

local function property_setter(prop)
	local attrs = property_atts(prop)
	if attrs.readonly then return end
	if not attrs.setter then
		local name = property_name(prop)
		attrs.setter = _('set%s%s:', name:sub(1,1):upper(), name:sub(2)) --'name' -> 'setName:'
	end
	return attrs.setter
end

local function property_ctype(prop)
	local attrs = property_attrs(prop)
	if not attrs.ctype then
		attrs.ctype = type_ctype(attrs.type) --cache it
	end
	return attrs.ctype
end

local function property_readonly(prop)
	return property_attrs(prop).readonly == true
end

local function property_ivar(prop)
	return property_attrs(prop).ivar
end

--methods

local function method_type(method)
	return ffi.string(C.method_getTypeEncoding(method))
end

local function method_selector(method)
	return ptr(C.method_getName(method))
end

local function method_name(method)
	return selector_name(method_selector(method))
end

local function method_object_ctype_string(method)
	return method_ctype_string(method_type(method))
end

local function method_object_ctype(method)
	return method_ctype(method_type(method))
end

local function method_implementation(method)
	return ffi.cast(method_object_ctype(method), C.method_getImplementation(method))
end

--classes

local function classes() --list all loaded classes
	return citer(own(C.objc_copyClassList(nil)))
end

local add_class_protocols --fw. decl.

local function class(name, super, proto, ...) --find or create a class

	if super == nil then --want to find a class, not to create one
		if type(name) ~= 'string' then return name end
		return ptr(C.objc_getClass(name))
	end

	--given a second arg., check for 'SuperClass <Prtocol1, Protocol2,...>' syntax
	if type(super) == 'string' then
		local supername, protos = super:match'^%s*([^%<%s]+)%s*%<%s*([^%>]+)%>%s*$'
		if supername then
			local t = {}
			for proto in (protos..','):gmatch'([^,%s]+)%s*,%s*' do
				t[#t+1] = proto
			end
			t[#t+1] = proto
			for i = 1, select('#', ...) do
				t[#t+1] = select(i, ...)
			end
			return class(name, supername, unpack(t))
		end
	end

	local superclass
	if super then
		superclass = class(super)
		check(superclass, 'superclass not found %s', super)
	end

	local cls = check(ptr(C.objc_allocateClassPair(superclass, name, 0)))
   C.objc_registerClassPair(cls)

	if proto then
		add_class_protocols(cls, proto, ...)
	end

	return cls
end

local function class_name(cls)
	return ffi.string(C.class_getName(cls))
end

local function superclass(cls)
	return ptr(C.class_getSuperclass(cls))
end

local function metaclass(cls)
	return ptr(C.object_getClass(ffi.cast(id_ctype, cls)))
end

local function issubclass(cls, ofcls)
	local super = superclass(cls)
	if super == ofcls then
		return true
	elseif not super then
		return false
	end
	return issubclass(super, ofcls)
end

local function class_protocols(cls) --does not include protocols of superclasses
	return citer(own(C.class_copyProtocolList(cls, nil)))
end

local function class_conforms(cls, proto)
	return C.class_conformsToProtocol(cls, proto) == 1
end

local function class_properties(cls) --inherited properties not included
	return citer(own(C.class_copyPropertyList(cls, nil)))
end

local function class_property(cls, name) --looks in superclasses too
	return ptr(C.class_getProperty(cls, name))
end

local function class_methods(cls, inst) --inherited methods not included
	if not inst then
		return class_methods(metaclass(cls), true)
	end
	return citer(own(C.class_copyMethodList(cls, nil)))
end

local function class_method(cls, sel, inst) --looks for inherited methods too
	if not inst then
		return class_method(metaclass(cls), sel, true)
	end
	return ptr(C.class_getInstanceMethod(cls, sel))
end

local function class_add_method(cls, sel, inst, mtype, func)
	if not inst then
		return class_add_method(metaclass(cls), sel, true, mtype, func)
	end
	if logtopics.method_ctype then
		log('method_ctype', '%s %s %s %s', class_name(cls), selector_name(sel), tostring(inst), method_ctype_string(mtype))
	end
	local ctype = method_ctype(mtype)
	local callback = ffi.cast(ctype, func) --note: these callback objects can't be released
	local imp = ffi.cast('IMP', callback)
	C.class_replaceMethod(cls, sel, imp, mtype) --add or replace
	invalidate_class(cls)
end

function add_class_protocols(cls, proto, ...)
	C.class_addProtocol(class(cls), protocol(proto))
	if ... then
		add_class_protocols(cls, ...)
	end
end

--find a selector in conforming protocols and if found, return its type
function conforming_method_type(cls, sel, inst)
	for proto in class_protocols(cls) do
		local mtype =
			protocol_method_type(proto, sel, inst, false) or
			protocol_method_type(proto, sel, inst, true)
		if mtype then
			return mtype
		end
	end
	if superclass(cls) then
		return conforming_method_type(superclass(cls), sel, inst)
	end
end

--class/instance/protocol method finding based on loose selector names.
--loose selector names are those that may or may not contain a trailing '_'.

local function find_method(cls, selname, inst)
	local sel = selector(selname)
	local meth = class_method(cls, sel, inst)
	if meth then return sel, meth end
	if not selname:find'[_%:]$' then --method not found, try again with a trailing '_'
		return find_method(cls, selname..'_', inst)
	end
end

local function find_conforming_method_type(cls, selname, inst)
	local sel = selector(selname)
	local mtype = conforming_method_type(cls, sel, inst)
	if mtype then return sel, mtype end
	if not selname:find'[_%:]$' then --method not found, try again with a trailing '_'
		return find_conforming_method_type(cls, selname..'_', inst)
	end
end

--class/instance method caller based on loose selector names

local _instance_vars = {} --{[nptr(obj)] = {var1 = val1, ...}}

local function collect_object(obj) --note: assume that this will be called multiple times on the same obj!
	obj:release()
	_instance_vars[nptr(obj)] = nil
end

--methods for which we should refrain from retaining the result object
noretain = {release=1, autorelease=1, retain=1, alloc=1, new=1, copy=1, mutableCopy=1}

--advanced sorcery note: ffi.gc() applies to cdata objects, not to the identities that they hold,
--so if you get the same object from two different invocations, you now have two cdata and your finalizer
--that you set with ffi.gc() will be called twice, each time when each cdata gets collected.

--so it's ok to own each new cdata that retain() returns, as long as we disown it on release().

local function method_caller(cls, selname, inst) --method caller based on a loose selector
	local sel, method = find_method(cls, selname, inst)
	if not sel then return end

	local imp = method_implementation(method)
	local can_retain = not noretain[selname]
	local is_release = selname == 'release' or selname == 'autorelease'
	local must_disown = is_release or selname == 'dealloc'

	return function(obj, ...) --note: obj is the class for a class method
		local ok, ret = pcall(imp, ffi.cast(id_ctype, obj), sel, ...)
		if not ok then
			check(false, '[%s %s] %s\n%s', tostring(cls), tostring(sel), ret, debug.traceback())
		end
		if is_release and logtopics.release then
			log('release', '%s, refcount after: %d', tostring(obj), tonumber(obj:retainCount()))
		end
		if must_disown then
			ffi.gc(obj, nil) --prevent calling obj:release() again on gc
		end
		if ret == nil then
			return nil --NULL -> nil
		end
		if can_retain and ffi.istype(id_ctype, ret) then
			ret = ret:retain()
			if logtopics.retain then
				log('retain', '%s, refcount after: %d', tostring(obj), tonumber(obj:retainCount()))
			end
		end
		if not must_disown and ffi.istype(id_ctype, ret) then
			ret = ffi.gc(ret, collect_object)
		end
		return ret
	end
end

local im_cache = {}

local instance_method_caller = memoize2(function(cls, selname) --caching to prevent creating a new caller on each call
	return method_caller(cls, selname, true)
end, im_cache)

local cm_cache = {}

local class_method_caller = memoize2(function(cls, selname) --caching to prevent creating a new caller on each call
	return method_caller(cls, selname, false)
end, cm_cache)

local function class_method_caller(cls, selname)
	return instance_method_caller(metaclass(cls), selname)
end

local function clear_cache(cls)
	cm_cache[nptr(cls)] = nil
	im_cache[nptr(cls)] = nil
end
function invalidate_class(cls) --required if overriding a method that was already called once
	clear_cache(cls)
	for acls in classes() do
		if issubclass(acls, cls) then
			clear_cache(acls)
		end
		if metaclass(acls) == cls then
			clear_cache(acls)
		end
	end
end

--class/instance lua vars

local _class_vars = {} --{[nptr(cls)] = {var1 = val1, ...}}

local function get_var(namespace)
	return function(obj, var)
		local vars = namespace[nptr(obj)]
		return vars and vars[var]
	end
end
local class_var    = get_var(_class_vars)
local instance_var = get_var(_instance_vars)

local function set_var(namespace)
	return function(obj, var, val)
		local nobj = nptr(obj)
		local vars = namespace[nobj]
		if not vars then
			vars = {}
			namespace[nobj] = vars
		end
		vars[var] = val
	end
end
local set_class_var    = set_var(_class_vars)
local set_instance_var = set_var(_instance_vars)

--class fields

--try different things in order, to create the effect of inherited namespaces.
local function get_class_field(cls, field)
	assert(cls ~= nil, 'attempt to index a NULL class')
	--look for an existing lua var
	local val = class_var(cls, field)
	if val ~= nil then
		return val
	end
	--look for a class property
	local prop = class_property(cls, field)
	if prop then
		local caller = class_method_caller(cls, property_getter(prop))
		if caller then --the getter is a class method so this is a "class property"
			return caller(cls)
		end
	end
	--look for a class method
	return class_method_caller(cls, field)
end

local function override(cls, selname, inst, func)
	--look to override an existing method
	local sel, method = find_method(cls, selname, inst)
	if sel then
		class_add_method(cls, sel, inst, method_type(method), func)
		return true
	end
	--look to override/create a conforming method
	local sel, mtype = find_conforming_method_type(cls, selname, inst)
	if sel then
		class_add_method(cls, sel, inst, mtype, func)
		return true
	end
end

--try different things in order, to create the effect of inherited namespaces.
local function set_class_field(cls, field, val)
	assert(cls ~= nil, 'attempt to index a NULL class')
	--look to set an existing lua var
	if class_var(cls, field) ~= nil then
		set_class_var(cls, field, val)
		return
	end
	--look to set a writable class property
	local prop = class_property(cls, field)
	if prop then
		local setter = property_setter(prop)
		if setter then --not read-only
			local caller = class_method_caller(cls, setter)
			if caller then --the setter is a class method so this is a "class property"
				caller(cls, val)
				return
			end
		end
	end
	--look to override an instance or class method
	if override(cls, field, true, val) then return end
	if override(cls, field, false, val) then return end
	--finally, add a new lua var
	set_class_var(cls, field, val)
end

ffi.metatype('struct objc_class', {
	__tostring = class_name,
	__index = get_class_field,
	__newindex = set_class_field,
})

--instance fields

--try different things in order, to create the effect of inherited namespaces.
local function get_instance_field(obj, field)
	assert(obj ~= nil, 'attempt to index a NULL object')
	--look for an existing instance lua var
	local val = instance_var(obj, field)
	if val ~= nil then
		return val
	end
	local prop = class_property(obj.isa, field)
	if prop then
		--look for an instance property
		local caller = instance_method_caller(obj.isa, property_getter(prop))
		if caller then --the getter is an instance method so this is an "instance property"
			return caller(obj)
		end
	end
	--look for an instance method
	local meth = instance_method_caller(obj.isa, field)
	if meth then return meth end
	--finally, look for a class field
	return get_class_field(obj.isa, field)
end

--try different things in order, to create the effect of inherited namespaces.
local function set_instance_field(obj, field, val)
	assert(obj ~= nil, 'attempt to index a NULL object')
	--look to set an existing lua var
	if instance_var(obj, field) ~= nil then
		set_instance_var(obj, field, val)
		return
	end
	--look to set a writable instance property
	local prop = class_property(obj.isa, field)
	if prop then
		local setter = property_setter(prop)
		if setter then --not read-only
			local caller = instance_method_caller(obj.isa, setter)
			if caller then --the setter is a class method so this is a "class property"
				caller(obj, val)
				return
			end
		end
	end
	--finally, add a new lua var
	set_instance_var(obj, field, val)
end

local function object_tostring(obj)
	if obj == nil then return 'nil' end
	return _('<%s>0x%x', class_name(obj.isa), nptr(obj))
end

ffi.metatype('struct objc_object', {
	__tostring = object_tostring,
	__index = get_instance_field,
	__newindex = set_instance_field,
})

--inspection -------------------------------------------------------------------------------------------------------------

--pretty helpers

local function p(...) --formatted line
	print(_(...))
end

local function hr() --horizontal line
	print(('-'):rep(100))
end

local function header(...) --formatted header
	print''
	hr'+'
	p('| '.._(...))
	hr'+'
end

local comp = memoize(function(getname)
	return function(a, b)
		return getname(a) < getname(b)
	end
end)
local function sorta(a, getname) --sort a C array by name
	local t = {}
	for i,e in apairs(a) do
		t[#t+1] = e
	end
	table.sort(t, comp(getname or tostring))
	return t
end

--pretty chunks

local function inspect_properties(props)
	if not props then return end
	p('\n%-40s %-20s %-40s %-40s', 'Properties:', 'ctype', 'getter', 'setter')
	hr()
	for i,prop in ipairs(sorta(props)) do
		p('%-40s %-20s %-40s %-40s', property_name(prop),
			type_ctype(property_ctype(prop)),
			property_getter(prop),
			property_setter(prop) or 'n/a')
	end
end

local function inspect_method_types(title, proto, types)
	if not next(types) then return end
	p('\n%-60s %s', title, 'ctype')
	hr()
	local t = {}
	for name in pairs(types) do
		t[#t+1] = name
	end
	table.sort(t)
	for i,name in ipairs(t) do
		p('%-60s %s', name, method_ctype_string(types[name]))
	end
end

local function inspect_methods(title, methods)
	if not methods then return end
	p('\n%-60s %s', title, 'ctype')
	hr()
	for meth in methods do
		p('%-60s %s', method_name(meth), method_ctype_string(meth))
	end
end

local function protocols_spec(protocols)
	local t = {}
	for i,proto in apairs(protocols) do
		t[#t+1] = protocol_name(proto) .. protocols_spec(protocol_protocols(proto))
	end
	return #t > 0 and _(' <%s>', table.concat(t, ', ')) or ''
end

local function class_spec(cls, indent)
	indent = indent or 1
	local super_spec = superclass(cls) and
		_('\n|%s<- %s', ('\t'):rep(indent), class_spec(superclass(cls), indent + 1)) or ''
	return class_name(cls) .. protocols_spec(class_protocols(cls)) .. super_spec
end

local function protocol_spec(proto)
	return protocol_name(proto) .. protocols_spec(protocol_protocols(proto))
end

--inspection api

local inspect = {}

local function filter(a, patt)
	local t = {}
	for i,e in ipairs(sorta(a)) do
		e = tostring(e)
		if not e:find'^_' and (not patt or e:find(patt)) then
			t[#t+1] = e
		end
	end
	return t
end

local function inspect_names(title, t, f)
	if #t == 0 then return end
	f = f or tostring
	p(title)
	for i,s in ipairs(t) do
		print('', f(s))
	end
end

function inspect.findmethod(cls, patt, methods, t)
	if not methods then
		local t = {}
		inspect.findmethod(cls, patt, instance_methods, t)
		inspect.findmethod(cls, patt, class_methods, t)
		inspect_names(_('Class %s:', tostring(cls)), t, function(meth)
			return _('%-60s %s', tostring(meth), method_ctype_string(method_type(meth)))
		end)
		return
	end
	for i,meth in apairs(methods(class(cls), inst)) do
		local name = tostring(meth)
		if not name:find'^_' and (not patt or name:find(patt)) then
			t[#t+1] = meth
		end
	end
end

function inspect.find(patt)
	--TODO: find framework
	inspect_names('Classes:', filter(classes(), patt))
	inspect_names('Protocols:', filter(protocols(), patt))
	for i,cls in ipairs(sorta(classes())) do
		if not tostring(cls):find'^_' then
			inspect.findmethod(cls, patt)
		end
	end
end

function inspect.protocol(proto)
	proto = protocol(proto)
	header('Protocol %s%s', protocol_name(proto), protocols_spec(protocol_protocols(proto)))
	inspect_properties(protocol_properties(proto))
	inspect_method_types('Instance Methods (required):', proto, protocol_methods(proto, true,  true))
	inspect_method_types('Instance Methods (optional):', proto, protocol_methods(proto, true,  false))
	inspect_method_types('Class Methods (required):',    proto, protocol_methods(proto, false, true))
	inspect_method_types('Class Methods (optional):',    proto, protocol_methods(proto, false, false))
end

function inspect.class_header(cls)
	header('Class %s', class_spec(class(cls)))
end

function inspect.class(cls)
	cls = class(cls)
	inspect.class_header(cls)
	inspect_properties(properties(cls))
	inspect_methods('Instance Methods:', class_methods(cls, true))
	inspect_methods('Class Methods:',    class_methods(cls, false))
end

function inspect.conforms(cls)
	local t = {} --{sel = protocol}
	for i,proto in apairs(class_protocols(cls)) do
		for i,sel in protocol_method_types(proto) do
			t[nptr(sel)] = proto
		end
	end
end

--publish everything

objc.C = C
objc.debug = P
objc.load = load_framework

ffi.metatype('struct objc_selector', {
	__tostring = selector_name,
	__index = {
		name = selector_name,
	},
})

ffi.metatype('struct Protocol', {
	__tostring = protocol_name,
	__index = {
		name = protocol_name,
		protocols = protocol_protocols,
		properties = protocol_properties,
		property = protocol_property,
		methods = protocol_methods,
		method_type = protocol_method_type,
		search = protocol_search,
	},
})

ffi.metatype('struct objc_property', {
	__tostring = property_name,
	__index = {
		name = property_name,
		getter = property_getter,
		setter = property_setter,
		ctype = property_ctype,
		readonly = property_readonly,
		ivar = property_ivar,
	},
})

ffi.metatype('struct objc_method', {
	__tostring = method_name,
	__index = {
		type = method_type,
		selector = method_selector,
		name = method_name,
		ctype_string = method_object_ctype_string,
		ctype = method_object_ctype,
		implementation = method_implementation,
	},
})

objc.selector = selector
objc.protocols = protocols
objc.protocol = protocol
objc.classes = classes
objc.class = class
objc.class_name = class_name
objc.superclass = superclass
objc.metaclass = metaclass
objc.issubclass = issubclass
objc.class_protocols = class_protocols
objc.class_conforms = class_conforms
objc.class_properties = class_properties
objc.class_property = class_property
objc.class_methods = class_methods
objc.class_method = class_method
objc.conform = add_class_protocols
objc.override = override

setmetatable(objc, {
	__index = function(t, k)
		return class(k) or C[k]
	end,
})

if not ... then
	for k,v in pairs(objc) do
		print(_('%-10s %s', type(v), 'objc.'..k))
	end
	for k,v in pairs(P) do
		print(_('%-10s %s', type(v), 'objc.debug.'..k))
	end
end

return objc
