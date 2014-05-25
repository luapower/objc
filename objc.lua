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
const char *ivar_getName(Ivar ivar);
const char *ivar_getTypeEncoding(Ivar ivar);
ptrdiff_t ivar_getOffset(Ivar ivar);
BOOL class_addIvar(Class cls, const char *name, size_t size, uint8_t alignment, const char *types);

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
Ivar * class_copyIvarList(Class cls, unsigned int *outCount);

//messages
id objc_msgSend(id theReceiver, SEL theSelector, ...);
]]

local C = ffi.C                               --C namespace
local P = setmetatable({}, {__index = _G})    --private namespace
local objc = {}                               --public namespace
setfenv(1, P)                                 --non-locals go in P

--helpers ----------------------------------------------------------------------------------------------------------------

local _ = string.format
local id_ctype = ffi.typeof'id'
local intptr_ctype = ffi.typeof'intptr_t'

local function ptr(p) --convert NULL pointer to nil for easier handling (say 'not ptr' instead of 'ptr == nil')
	if p == nil then return nil end
	return p
end

local function nptr(p) --convert pointer to lua number for using as table key
	if p == nil then return nil end
	local np = ffi.cast(intptr_ctype, p)
	local n = tonumber(np)
	assert(ffi.cast(intptr_ctype, n) == np) --check that we don't get pointers in the upper 13 bits on 64bit
	return n
end

local function own(p) --own a malloc()'ed pointer
	return p ~= nil and ffi.gc(p, C.free) or nil
end

local function csymbol_(name) return C[name] end
local function csymbol(name)
	local ok, sym = pcall(csymbol_, name)
	if not ok then return end
	return sym
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
		if a[i] == nil then return end
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
printcdecl = false --print C declarations to stdout (then you can grab them and make static ffi headers)
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
		if printcdecl then
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

--type encodings (conversion to C type declarations) ---------------------------------------------------------------------

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
local function struct_ctype(s, name, deftype, indent) --('{CGPoint="x"d"y"d}', 'NSPoint') -> 'struct CGPoint NSPoint'

	--break the struct/union def. in its constituent parts: keyword, tag, fields
	local kw, tag, fields = s:match'^(.)([^=]*)=?(.*).$' -- '{name=fields}'
	kw = kw == '{' and 'struct' or 'union'
	if tag == '?' or tag == '' then tag = nil end -- ? or empty means anonymous struct
	if fields == '' then fields = nil end -- empty definition means opaque struct

	if not fields and not tag then --rare case: '{?}' coming from '^{?}'
		return 'void'..optname(name)
	end

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
			table.insert(t, type_ctype(s, name, 'cdef', true)) --eg. 'struct _NSPoint origin'
			return '' --remove the match
		end
		local s = fields
		local n
		while s ~= '' do
			               s,n = s:gsub('^"([^"]*)"([%^]*%b{})',     addfield)     --try "field"{...}
			if n == 0 then s,n = s:gsub('^"([^"]*)"([%^]*%b())',     addfield) end --try "field"(...)
			if n == 0 then s,n = s:gsub('^"([^"]+)"([%^]*%b[])',     addfield) end --try "field"[...]
			if n == 0 then s,n = s:gsub('^"([^"]+)"(@)%?',           addfield) end --try "field"@? (block type)
			if n == 0 then s,n = s:gsub('^"([^"]+)"(@"[A-Z][^"]+")', addfield) end --try "field"@"Class"
			if n == 0 then s,n = s:gsub('^"([^"]*)"([^"]+)',         addfield) end --try "field"...
			assert(n > 0, s)
		end
		local ctype = _('%s%s {\n\t%s;\n}', kw, optname(tag), table.concat(t, ';\n\t'))

		--anonymous struct: return the full definition
		if not tag then
			if indent then --this is the only multiline output that can be indented
				ctype = ctype:gsub('\n', '\n\t')
			end
			return _('%s%s', ctype, optname(name))
		end

		--named struct: cdef it.
		--note: duplicate struct cdefs are rejected by luajit 2.0 with an error. we guard against that.
		declare(tag, 'struct', ctype .. ';')
	end

	return _('%s %s%s', kw, tag, optname(name))
end

local function bitfield_ctype(s, name, deftype) --('bN', 'name') -> 'unsigned name: N'; N must be <= 32
	local n = s:match'^b(%d+)$'
	return _('unsigned %s: %d', name or '_', n)
end

local function pointer_ctype(s, name, ...) --('^type', 'name') -> ctype('type', '*name')
	return type_ctype(s:sub(2), '*'..(name or ''), ...)
end

local function char_ptr_ctype(s, ...) --('*', 'name') -> 'char *name'
	return pointer_ctype('^c', ...)
end

local function primitive_ctype(ctype)
	return function(s, name)
		return ctype .. optname(name)
	end
end

function const_ctype(s, ...)
	return 'const ' .. type_ctype(s:sub(2), ...)
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

	['@'] = primitive_ctype'id', --@ or @? or @"ClassName"
	['#'] = primitive_ctype'Class',
	[':'] = primitive_ctype'SEL',

	['['] = array_ctype,    -- [Ntype]         ; N = number of elements
	['{'] = struct_ctype,   -- {name=fields}   ; struct
	['('] = struct_ctype,   -- (name=fields)   ; union
	['b'] = bitfield_ctype, -- bN              ; N = number of bits
	['^'] = pointer_ctype,  -- ^type           ; pointer
	['*'] = char_ptr_ctype, -- *               ; char* pointer
	['r'] = const_ctype,
}

--convert a type encoding to a C type (as string)
--3rd arg = 'cdef' means that named structs contain field names and thus can and should be cdef'ed before returning.
function type_ctype(s, name, ...)
	local decoder = assert(ctype_decoders[s:sub(1,1)], s)
	return decoder(s, name, ...)
end

--decode a method type encoding returning the C type for the ret. val and a list of C types for the arguments
local function method_arg_ctypes(s, forcallback) --eg. 'v12@0:4c8' (retval offset arg1 offset arg2 offset ...)
	local ret_ctype
	local arg_ctypes = {}
	local stop
	local function addarg(s)
		if stop then return '' end
		--ffi callbacks don't work with pass-by-value structs, so we're going to stop at the first one.
		local struct_byval = forcallback and s:find'^r?[{%(]'
		if not ret_ctype then
			ret_ctype = struct_byval and 'void' or type_ctype(s)
		elseif not struct_byval then
			table.insert(arg_ctypes, type_ctype(s))
		else
			stop = true
		end
		return '' --remove the match
	end
	local n
	while s ~= '' do
		               s,n = s:gsub('^[nNoORV]?(r?[%^]*%b{})%d*',     addarg)     --try {...}offset
		if n == 0 then s,n = s:gsub('^[nNoORV]?(r?[%^]*%b())%d*',     addarg) end --try (...)offset
		if n == 0 then s,n = s:gsub('^[nNoORV]?(r?[%^]*%b[])%d*',     addarg) end --try [...]offset
		if n == 0 then s,n = s:gsub('^[nNoORV]?(r?@)%?%d*',           addarg) end --try @? (block type)
		if n == 0 then s,n = s:gsub('^[nNoORV]?(r?@"[A-Z][^"]+")%d*', addarg) end --try @"Class"offset
		if n == 0 then s,n = s:gsub('^[nNoORV]?(r?[%^]*[cislqCISLQfdBv%?@#%:%*])%d*', addarg) end --try <primitive>offset
		assert(n > 0, s)
	end
	return ret_ctype, arg_ctypes
end

--convert a method type encoding to a C function type (as string)
local function method_ctype_string(s, ...) --eg. 'v12@0:4c8' (retval offset arg1 offset arg2 offset ...)
	local ret_ctype, arg_ctypes = method_arg_ctypes(s, ...)
	return _('%s (*) (%s)', ret_ctype, table.concat(arg_ctypes, ', '))
end

local method_ctype = memoize(function(s) --caching to prevent ctype duplicates and avoid reparsing
	return ffi.typeof(method_ctype_string(s))
end)

local function callback_method_ctype_string(s) --callback types are more limited
	return method_ctype_string(s, true)
end

local callback_method_ctype = memoize(function(s)
	return ffi.typeof(callback_method_ctype_string(s))
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
	ftag.retval = type_ctype(s)
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

function tag.function_alias(attrs) --these tags always come after the 'function' tags
	local name = attrs.name
	local original = attrs.original
	if lazyfuncs then
		--delay getting a cdef to the original function until the first call to the alias
		rawset(objc, name, function(...)
			local func = C[original]
			rawset(objc, name, func) --replace this wrapper with the original function
			return func(...)
		end)
	else
		if csymbol(original) then
			rawset(objc, name, C[original])
		else
			err('alias', 'symbol not found %s for %s', original, name)
		end
	end
end

local new_informal_protocol --fw. decl.

local proto --informal_protocol object for the current 'informal_protocol' tag

function tag.informal_protocol(attrs)
	proto = new_informal_protocol(attrs.name, attrs.class_method ~= 'true')
end

function tag.method(attrs)
	if not proto then return end
	local s = attrs[typekey] or attrs.type
	if not s then return end --type not available on this platform
	proto._methods[attrs.selector] = s
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
		--load the framework binary which contains classes, functions and protocols
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

ffi.metatype('struct objc_selector', {
	__tostring = selector_name,
	__index = {
		name = selector_name,
	},
})

--formal protocols

local function formal_protocols()
	return citer(own(C.objc_copyProtocolList(nil)))
end

local function formal_protocol(name)
	return ptr(C.objc_getProtocol(name))
end

local function formal_protocol_name(proto)
	return ffi.string(C.protocol_getName(proto))
end

local function formal_protocol_protocols(proto) --protocols of superprotocols not included
	return citer(own(C.protocol_copyProtocolList(proto, nil)))
end

local function formal_protocol_properties(proto) --inherited properties not included
	return citer(own(C.protocol_copyPropertyList(proto, nil)))
end

local function formal_protocol_property(proto, name, required, readonly) --looks in superprotocols too
	return ptr(C.protocol_getProperty(proto, name, required, readonly))
end

local function formal_protocol_methods(proto, inst, required) --inherited methods not included
	local desc = own(C.protocol_copyMethodDescriptionList(proto, required, inst, nil))
	local i = -1
	return function()
		i = i + 1
		if desc == nil then return end
		if desc[i].name == nil then return end
		return desc[i].name, ffi.string(desc[i].types)
	end
end

local function formal_protocol_method_type(proto, sel, inst, required) --looks in superprotocols too
	local desc = C.protocol_getMethodDescription(proto, sel, required, inst)
	if desc.name == nil then return end
	return ffi.string(desc.types)
end

ffi.metatype('struct Protocol', {
	__tostring = formal_protocol_name,
	__index = {
		name        = formal_protocol_name,
		protocols   = formal_protocol_protocols,
		properties  = formal_protocol_properties,
		property    = formal_protocol_property,
		methods     = formal_protocol_methods,
		method_type = formal_protocol_method_type,
	},
})

--informal protocols (must have the exact same API as formal protocols)

local _informal_protocols = {} --{name = proto}
local infprot = {}
local infprot_meta = {__index = infprot}

local function informal_protocols()
	return pairs(_informal_protocols)
end

local function informal_protocol(name)
	return _informal_protocols[name]
end

function new_informal_protocol(name, inst)
	if formal_protocol(name) then return end --prevent needless duplication of formal protocols
	local proto = setmetatable({_name = name, _inst = inst, _methods = {}}, infprot_meta)
	_informal_protocols[name] = proto
	return proto
end

function infprot:name()
	return self._name
end

function noop() return end
function infprot:protocols()
	return noop
end

function infprot:properties()
	return noop
end

infprot.property = noop

function infprot:methods()
	return pairs(self._methods)
end

function infprot:method_type(sel, inst, required)
	if required then return end --by definition, informal protocols do not contain required methods
	if self._inst ~= inst then return end
	return self._methods[selector_name(sel)]
end

--all protocols

local function protocols() --list all loaded protocols
	return coroutine.wrap(function()
		for proto in formal_protocols() do
			coroutine.yield(proto)
		end
		for name, proto in informal_protocols() do
			coroutine.yield(proto)
		end
	end)
end

local function protocol(name) --protocol by name
	if type(name) ~= 'string' then return name end
	return check(formal_protocol(name) or informal_protocol(name), 'unknown protocol %s', name)
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
	local attrs = property_attrs(prop)
	if not attrs.getter then
		attrs.getter = property_name(prop) --default getter; cache it
	end
	return attrs.getter
end

local function property_setter(prop)
	local attrs = property_attrs(prop)
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

ffi.metatype('struct objc_property', {
	__tostring = property_name,
	__index = {
		name     = property_name,
		getter   = property_getter,
		setter   = property_setter,
		ctype    = property_ctype,
		readonly = property_readonly,
		ivar     = property_ivar,
	},
})

--methods

local function method_selector(method)
	return ptr(C.method_getName(method))
end

local function method_name(method)
	return selector_name(method_selector(method))
end

local function method_object_type(method)
	return ffi.string(C.method_getTypeEncoding(method))
end

local function method_object_ctype_string(method)
	return method_ctype_string(method_object_type(method))
end

local function method_object_ctype(method)
	return method_ctype(method_object_type(method))
end

local function method_implementation(method)
	return ffi.cast(method_object_ctype(method), C.method_getImplementation(method))
end

ffi.metatype('struct objc_method', {
	__tostring = method_name,
	__index = {
		selector       = method_selector,
		name           = method_name,
		type           = method_object_type,
		ctype_string   = method_object_ctype_string,
		ctype          = method_object_ctype,
		implementation = method_implementation,
	},
})

--classes

local function classes() --list all loaded classes
	return citer(own(C.objc_copyClassList(nil)))
end

local class_add_protocols --fw. decl.

local function class(name, super, proto, ...) --find or create a class

	if super == nil then --want to find a class, not to create one
		if type(name) ~= 'string' then return name end
		return ptr(C.objc_getClass(name))
	else
		assert(type(name) == 'string', 'class name expected')
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

	check(not class(name), 'class already defined %s', name)

	local cls = check(ptr(C.objc_allocateClassPair(superclass, name, 0)))
   C.objc_registerClassPair(cls)

	if proto then
		class_add_protocols(cls, proto, ...)
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
	return ptr(cls.isa)
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

--class protocols

local class_informal_protocols = {} --{[nptr(cls)] = {name = informal_protocol,...}}

local function class_protocols(cls) --does not include protocols of superclasses
	return coroutine.wrap(function()
		for proto in citer(own(C.class_copyProtocolList(cls, nil))) do
			coroutine.yield(proto)
		end
		local t = class_informal_protocols[nptr(cls)]
		if not t then return end
		for name, proto in pairs(t) do
			coroutine.yield(proto)
		end
	end)
end

local function class_conforms(cls, proto)
	if C.class_conformsToProtocol(cls, protocol(proto)) == 1 then
		return true
	end
	local t = class_informal_protocols[nptr(cls)]
	return t and t[protocol(proto):name()] and true or false
end

function class_add_protocols(cls, proto, ...)
	proto = protocol(proto)
	if type(proto) == 'table' then
		local t = class_informal_protocols[nptr(cls)]
		if not t then
			t = {}
			class_informal_protocols[nptr(cls)] = t
		end
		t[proto:name()] = proto
	else
		C.class_addProtocol(class(cls), proto)
	end
	if ... then
		class_add_protocols(cls, ...)
	end
end

--find a selector in conforming protocols and if found, return its type
function conforming_method_type(cls, sel, inst)
	for proto in class_protocols(cls) do
		local mtype =
			proto:method_type(sel, inst, false) or
			proto:method_type(sel, inst, true)
		if mtype then
			return mtype
		end
	end
	if superclass(cls) then
		return conforming_method_type(superclass(cls), sel, inst)
	end
end

--class properties

local function class_properties(cls) --inherited properties not included
	return citer(own(C.class_copyPropertyList(cls, nil)))
end

local function class_property(cls, name) --looks in superclasses too
	return ptr(C.class_getProperty(cls, name))
end

--class methods

local function class_methods(cls) --inherited methods not included
	return citer(own(C.class_copyMethodList(cls, nil)))
end

local function class_method(cls, sel) --looks for inherited methods too
	return ptr(C.class_getInstanceMethod(cls, sel))
end

local clear_cache --fw. decl.

local function add_class_method(cls, sel, func, mtype)
	sel = selector(sel)
	local ctype = callback_method_ctype(mtype)
	local callback = ffi.cast(ctype, func) --note: these callback objects can't be released
	local imp = ffi.cast('IMP', callback)
	C.class_replaceMethod(cls, sel, imp, mtype) --add or replace
	clear_cache(cls)
	if logtopics.add_class_method then
		log('add_class_method', '%-40s %-40s %s', selector_name(sel), mtype, callback_method_ctype_string(mtype))
	end
end

--ivars

local function class_ivars(cls)
	return citer(own(C.class_copyIvarList(cls, nil)))
end

local function instance_ivar(obj, name)
	return ptr(C.object_getInstanceVariable(obj, name, nil))
end

local function ivar_name(ivar)
	return ffi.string(C.ivar_getName(ivar))
end

local function ivar_offset(ivar) --this could be just an alias but we want to load this module in windows too
	return C.ivar_getOffset(ivar)
end

local function ivar_type(ivar)
	return ffi.string(C.ivar_getTypeEncoding(ivar))
end

local function ivar_ctype_string(ivar) --NOTE: bitfield ivars not supported (need ivar layouts for that)
	local itype = ivar_type(ivar):match'^[rnNoORV]?(.*)'
	return type_ctype('^'..itype, nil, itype:find'^[%{%(]%?' and 'cdef')
end

local ivar_ctypes = {} --{[nptr(cls)] = {ivar_name = ctype, ...}}

local function ivar_ctype(cls, name, ivar)
	local t = ivar_ctypes[nptr(cls)]
	if not t then
		t = {}
		ivar_ctypes[nptr(cls)] = t
	end
	if t[name] then
		return t[name]
	end
	local ctype = ffi.typeof(ivar_ctype_string(ivar))
	t[name] = ctype
	return ctype
end

local byteptr_ctype = 'uint8_t*'

local function ivar_get_value(obj, name, ivar)
	return ffi.cast(ivar_ctype(obj.isa, name, ivar), ffi.cast(byteptr_ctype, obj) + ivar_offset(ivar))[0]
end

local function ivar_set_value(obj, name, ivar, val)
	ffi.cast(ivar_ctype(obj.isa, name, ivar), ffi.cast(byteptr_ctype, obj) + ivar_offset(ivar))[0] = val
end

ffi.metatype('struct objc_ivar', {
	__tostring = ivar_name,
	__index = {
		name = ivar_name,
		type = ivar_type,
		offset = ivar_offset,
		ctype = ivar_ctype_string,
	},
})

--class/instance lua vars

local luavars = {} --{[nptr(cls|obj)] = {var1 = val1, ...}}

local function get_luavar(obj, var)
	local vars = luavars[nptr(obj)]
	return vars and vars[var]
end

local function set_luavar(obj, var, val)
	local vars = luavars[nptr(obj)]
	if not vars then
		vars = {}
		luavars[nptr(obj)] = vars
	end
	vars[var] = val
end

--class/instance/protocol method finding based on loose selector names.
--loose selector names are those that may or may not contain a trailing '_'.

local function find_method(cls, selname)
	local sel = selector(selname)
	local meth = class_method(cls, sel)
	if meth then return sel, meth end
	if not selname:find'[_%:]$' then --method not found, try again with a trailing '_'
		return find_method(cls, selname..'_')
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

--NOTE: ffi.gc() applies to cdata objects, not to the identities that they hold. Thus you can easily get
--the same object from two different method invocations into two distinct cdata objects. Setting ffi.gc()
--on both will result in your finalizer being called twice, each time when each cdata gets collected.
--This means that references to objects need to be refcounted if per-object resources need to be released on gc.

local refcounts = {} --number of collectable cdata references to an object

local function add_refcount(obj, n)
	local refcount = (refcounts[nptr(obj)] or 0) + n
	if refcount == 0 then
		refcount = nil
	end
	refcounts[nptr(obj)] = refcount
	return refcount
end

local function release_object(obj)
	if not add_refcount(obj, -1) then
		luavars[nptr(obj)] = nil
	end
end

local function collect_object(obj) --note: assume that this will be called multiple times on the same obj!
	obj:release()
end

--methods for which we should refrain from retaining the result object
noretain = {release=1, autorelease=1, retain=1, alloc=1, new=1, copy=1, mutableCopy=1}

local caller_cache = {}

local method_caller = memoize2(function(cls, selname) --method caller based on a loose selector
	local sel, method = find_method(cls, selname)
	if not sel then return end

	local imp = method_implementation(method)
	local can_retain = not noretain[selname]
	local is_release = selname == 'release' or selname == 'autorelease'

	return function(obj, ...) --note: obj is the class for a class method
		if logtopics.method_call then
			log('method_call', '%-40s %-40s %s', selector_name(sel),
					method_object_type(method), method_object_ctype_string(method))
		end
		local ok, ret = pcall(imp, ffi.cast(id_ctype, obj), sel, ...)
		if not ok then
			check(false, '[%s %s] %s\n%s', tostring(cls), tostring(sel), ret, debug.traceback())
		end
		if is_release then
			ffi.gc(obj, nil) --disown this reference to obj
			release_object(obj)
			if logtopics.release then
				log('release', '%s, refcount after: %d', tostring(obj), tonumber(obj:retainCount()))
			end
		end
		if ret == nil then
			return nil --NULL -> nil
		end
		if can_retain and ffi.istype(id_ctype, ret) then
			ret = ret:retain() --retain() will make ret a strong reference so we don't have to
			if logtopics.retain then
				log('retain', '%s, refcount after: %d', tostring(obj), tonumber(obj:retainCount()))
			end
		elseif not is_release and ffi.istype(id_ctype, ret) then
			ffi.gc(ret, collect_object)
			add_refcount(ret, 1)
		end
		return ret
	end
end, caller_cache)

function clear_cache(cls)
	caller_cache[nptr(cls)] = nil
	ivar_ctypes[nptr(cls)] = nil
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

--add, replace or override an existing/conforming instance/class method based on a loose selector name
local function override(cls, selname, inst, func, mtype) --returns true if a method was found and created
	--look to override an existing method
	local targetcls = inst and cls or metaclass(cls)
	local sel, method = find_method(targetcls, selname)
	if sel then
		add_class_method(targetcls, sel, func, mtype or method_object_type(method))
		return true
	end
	--look to override/create a conforming method
	local sel, cmtype = find_conforming_method_type(cls, selname, inst)
	if sel then
		add_class_method(targetcls, sel, func, mtype or cmtype)
		return true
	end
end

--class fields

--try to get, in order:
--		a class luavar
--		a readable class property
--		a class method
local function get_class_field(cls, field)
	assert(cls ~= nil, 'attempt to index a NULL class')
	--look for an existing class lua var
	local val = get_luavar(cls, field)
	if val ~= nil then
		return val
	end
	--look for a class property
	local prop = class_property(cls, field)
	if prop then
		local caller = method_caller(metaclass(cls), property_getter(prop))
		if caller then --the getter is a class method so this is a "class property"
			return caller(cls)
		end
	end
	--look for a class method
	return method_caller(metaclass(cls), field)
end

-- try to set, in order:
--		an existing class luavar
--		a writable class property
--		an instance method
--		a conforming instance method
--		a class method
--		a conforming class method
local function set_existing_class_field(cls, field, val)
	--look to set an existing class lua var
	if get_luavar(cls, field) ~= nil then
		set_luavar(cls, field, val)
		return true
	end
	--look to set a writable class property
	local prop = class_property(cls, field)
	if prop then
		local setter = property_setter(prop)
		if setter then --not read-only
			local caller = method_caller(metaclass(cls), setter)
			if caller then --the setter is a class method so this is a "class property"
				caller(cls, val)
				return true
			end
		end
	end
	--look to override an instance/instance-conforming/class/class-conforming method, in this order
	if override(cls, field, true, val) then return true end
	if override(cls, field, false, val) then return true end
end

--try to set, in order:
--		an existing class field (see above)
--		a new class luavar
local function set_class_field(cls, field, val)
	assert(cls ~= nil, 'attempt to index a NULL class')
	--look to set an existing class field
	if set_existing_class_field(cls, field, val) then return end
	--finally, set a new class lua var
	set_luavar(cls, field, val)
end

ffi.metatype('struct objc_class', {
	__tostring = class_name,
	__index = get_class_field,
	__newindex = set_class_field,
})

--instance fields

--try to get, in order;
--		an instance luavar
--		a readable instance property
--		an ivar
--		an instance method
--		a class field (see above)
local function get_instance_field(obj, field)
	assert(obj ~= nil, 'attempt to index a NULL object')
	--shortcut: look for an existing instance lua var
	local val = get_luavar(obj, field)
	if val ~= nil then
		return val
	end
	--look for an instance property
	local prop = class_property(obj.isa, field)
	if prop then
		local caller = method_caller(obj.isa, property_getter(prop))
		if caller then --the getter is an instance method so this is an "instance property"
			return caller(obj)
		end
	end
	--look for an ivar
	local ivar = instance_ivar(obj, field)
	if ivar then
		return ivar_get_value(obj, field, ivar)
	end
	--look for an instance method
	local caller = method_caller(obj.isa, field)
	if caller then
		return caller
	end
	--finally, look for a class field
	return get_class_field(obj.isa, field)
end

--try to set, in order:
--		an existing instance luavar
--		a writable instance property
--		an ivar
--		an existing class field (see above)
--		a new instance luavar
local function set_instance_field(obj, field, val)
	assert(obj ~= nil, 'attempt to index a NULL object')
	--shortcut: look to set an existing instance lua var
	if get_luavar(obj, field) ~= nil then
		set_luavar(obj, field, val)
		return
	end
	--look to set a writable instance property
	local prop = class_property(obj.isa, field)
	if prop then
		local setter = property_setter(prop)
		if setter then --not read-only
			local caller = method_caller(obj.isa, setter)
			if caller then --the setter is an instance method so this is an "instance property"
				caller(obj, val)
				return
			end
		else
			check(false, 'attempt to write to read/only property "%s"', field)
		end
	end
	--look to set an ivar
	local ivar = instance_ivar(obj, field)
	if ivar then
		ivar_set_value(obj, field, ivar, val)
		return
	end
	--look to set an existing class field
	if set_existing_class_field(obj.isa, field, val) then return end
	--finally, add a new lua var
	set_luavar(obj, field, val)
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

--blocks -----------------------------------------------------------------------------------------------------------------

--http://clang.llvm.org/docs/Block-ABI-Apple.html

ffi.cdef[[
struct __block_descriptor_1 {
	unsigned long int reserved;                    // NULL
	unsigned long int size;                        // sizeof(struct __block_literal_1)
};

struct __block_literal_1 {
	struct __block_literal_1 *isa;
	int flags;
	int reserved;
	void *invoke;
	struct __block_descriptor_1 *descriptor;
};

struct __block_literal_1 *_NSConcreteGlobalBlock;
]]

local block_descriptor = ffi.new'struct __block_descriptor_1'
block_descriptor.reserved = 0
block_descriptor.size = ffi.sizeof'struct __block_literal_1'
local block_ctype = ffi.typeof'struct __block_literal_1'

--create a block and return it typecast to 'id' (also returns the callback object for freeing)
local function block(func, mtype)
	mtype = mtype or 'v'
	mtype = mtype:sub(1,1) .. '^v' .. mtype:sub(2)
	local function wrapper(block, ...)
		return func(...)
	end
	local callback = ffi.cast(callback_method_ctype(mtype), wrapper)
	local block = block_ctype()
	block.isa = C._NSConcreteGlobalBlock
	block.flags = bit.lshift(1, 29)
	block.reserved = 0
	block.invoke = ffi.cast('void*', callback)
	block.descriptor = block_descriptor
	return ffi.cast(id_ctype, block), callback
end

--lua type conversions ---------------------------------------------------------------------------------------------------

function toobj(v) --convert a lua value to an objc object representing that value
	if type(v) == 'number' then
		return objc.NSNumber:numberWithDouble(v)
	elseif type(v) == 'string' then
	  return objc.NSString:stringWithUTF8String(v)
	elseif type(v) == 'table' then
		if #v == 0 then
			 local dic = objc.NSMutableDictionary:dictionary()
			 for k,v in pairs(v) do
				  dic:setObject_forKey(toobj(v), toobj(k))
			 end
			 return dic
		else
			 local arr = objc.NSMutableArray:array()
			 for i,v in ipairs(v) do
				  arr:addObject(toobj(v))
			 end
			 return arr
		end
	elseif type(v) == 'function' then
		return (block(v))
	else
		return v --pass through
	end
end

local function tolua(obj) --convert an objc object that converts naturally to a lua value
	local classname = class_name(obj.isa)
	if issubclass(obj.isa, objc.NSNumber) then
		return obj:doubleValue()
	elseif issubclass(obj.isa, objc.NSString) then
		return ffi.string(obj:UTF8String())
	elseif issubclass(obj.isa, objc.NSDictionary) then
		local t = {}
		local count = obj:count()
		local vals = ffi.new('id[?]', count)
		local keys = ffi.new('id[?]', count)
		obj:getObjects_andKeys(vals, keys)
		for i = 0, count-1 do
			t[tolua(keys[i])] = tolua(vals[i])
		end
		return t
	elseif issubclass(obj.isa, objc.NSArray) then
		local t = {}
		for i = 0, obj:count()-1 do
			t[#t+1] = tolua(obj:objectAtIndex(i))
		end
		return t
	else
		return obj --pass through
	end
end

--publish everything -----------------------------------------------------------------------------------------------------

objc.C = C
objc.debug = P
objc.load = load_framework

objc.type_ctype = type_ctype
objc.method_ctype = method_ctype_string
objc.callback_method_ctype = callback_method_ctype_string

local function objc_protocols(cls)
	if not cls then
		return protocls()
	else
		return class_protocols(cls)
	end
end

objc.SEL = selector
objc.protocols = objc_protocols
objc.protocol = protocol
objc.classes = classes
objc.class = class
objc.classname = class_name
objc.superclass = superclass
objc.metaclass = metaclass
objc.issubclass = issubclass
objc.conforms = class_conforms
objc.properties = class_properties
objc.property = class_property
objc.methods = class_methods
objc.method = class_method
objc.ivars = class_ivars
objc.ivar = instance_ivar
objc.conform = class_add_protocols
objc.override = override
objc.addmethod = add_class_method
objc.invalidate = invalidate_class

objc.block = block
objc.toobj = toobj
objc.tolua = tolua

setmetatable(objc, {
	__index = function(t, k)
		return class(k) or csymbol(k)
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
