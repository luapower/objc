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
BOOL class_respondsToSelector(Class cls, SEL sel);

//methods
Method class_getClassMethod(Class aClass, SEL aSelector);
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

local C = ffi.C
local objc = {C = C}

objc.debug = {
	loadtypes = true,      --load bridgesupport file
	loaddeps = false,      --load dependencies specified in the bridgesupport file (usually too many to be useful)
	errors = true,         --log errors of any kind
	cdefs = false,         --print cdefs to stdout (then you can grab them and make your own static cdef headers)
	lazyfuncs = true,      --cdef functions on the first call rather than at the time of parsing the xml (see below)
	methodtypes = false,   --log method ctype parsing
	load = false,          --log loaded frameworks
	release = false,       --log relases with refcount
	retain = false,        --log retains with refcount
	redef = false,         --check incompatible redefinition attempts (makes parsing slower)
	stats = {
		errors = 0,         --number of cdef errors
		cdefs = 0,          --number of cdefs without error
		redef = 0,          --number of incompatible redefines (if debug.redef == true)
	}
}

--helpers ----------------------------------------------------------------------------------------------------------------

local id_ctype = ffi.typeof'id'

local function ptr(p) --convert NULL pointer to nil for easier handling (say 'not ptr' instead of 'ptr == nil')
	if p == nil then return nil end
	return p
end

local function nptr(p) --convert pointer to lua number for using as table key
	if p == nil then return nil end
	return tonumber(ffi.cast('intptr_t', p))
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

local function log(topic, fmt, ...) --debug logger
	io.stderr:write(string.format('[objc] %-16s %s\n', topic, string.format(fmt, ...)))
end

local function check(ok, fmt, ...)
	if ok then return ok end
	error(string.format(fmt or 'assertion failed!', ...))
end

local function canread(path) --check that a file is readable without having to open it
	return C.access(path, bit.lshift(1,2)) == 0
end

local function defined(name, namespace) --check if a name is already defined in a C namespace (structs or globals)
	return namespace[name] and not objc.debug.redef
end

local function redefined(name, namespace, new_cdecl) --check cdecl redefinitions and report on incompatible ones
	local old_cdecl = namespace[name]
	if not old_cdecl then return end
	if not objc.debug.redef then return end
	if old_cdecl == new_cdecl then return true end --already defined but same def.
	objc.debug.stats.redef = objc.debug.stats.redef + 1
	if objc.debug.errors then
		log('redefinition', '%s\nold:\n\t%s\nnew:\n\t%s', name, old_cdecl, new_cdecl)
	end
	return true
end

local function cdef(name, namespace, cdecl) --define a C type, const or function via ffi.cdef
	if redefined(name, namespace, cdecl) then return end
	local ok, err = pcall(ffi.cdef, cdecl)
	if ok then
		objc.debug.stats.cdefs = objc.debug.stats.cdefs + 1
		if objc.debug.cdefs then
			print(s)
		end
	else
		if err == 'table overflow' then --fatal error from luajit: no more space for ctypes
			error'too many ctypes'
		end
		objc.debug.stats.errors = objc.debug.stats.errors + 1
		if objc.debug.errors then
			log('cdef', '%s\n\t%s', err, cdecl)
		end
	end
	namespace[name] = objc.debug.redef and cdecl or true --only store the cdecl if needed
	return ok
end

--type parsing/conversion to C types -------------------------------------------------------------------------------------

local type_ctype --fw. decl.

local function optname(name) --format an optional name: if not nil, return it with a space in front
	return name and ' '..name or ''
end

local function array_ctype(s, name, ...) --('[Ntype]', 'name') -> ctype('type', 'name[N]')
	local n,s = s:match'^%[(%d+)(.-)%]$'
	name = string.format('%s[%d]', name or '', n)
	return type_ctype(s, name, ...)
end

local structs = {} --{tag = true or ctype}

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
		return string.format('%s %s%s', kw, tag, optname(name))
	end

	if not tag or not defined(tag, structs) then --anonymous or not alreay defined: parse it

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
		local ctype = string.format('%s%s {\n\t%s;\n}', kw, optname(tag), table.concat(t, ';\n\t'))

		--anonymous struct: return the full definition
		if not tag then
			return string.format('%s%s', ctype, optname(name))
		end

		--named struct: cdef it.
		--note: duplicate struct cdefs are rejected by luajit 2.0 with an error.
		cdef(tag, structs, ctype .. ';')
	end

	return string.format('%s %s%s', kw, tag, optname(name))
end

local function bitfield_ctype(s, name) --('bN', 'name') -> 'unsigned int name: N'
	local n = s:match'^b(%d+)$'
	return string.format('unsigned int %s: %d', name or '_', n)
end

local function pointer_ctype(s, name, deftype) --('^type', 'name') -> ctype('type', '(*name)')
	if deftype == 'retval' then
		name = string.format('*%s', name or '') --special case for function return values
	else
		name = string.format('(*%s)', name or '') --make '(*int)[8]' instead of the ambiguous '*int[8]'
	end
	return type_ctype(s:sub(2), name, deftype)
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

--convert a runtime type encoding to a C type (as string)
--deftype = 'cdef' means that named structs should be cdef'ed before returning.
--deftype = 'retval' means it's a return-value type, so don't protect pointers with '(*name)'
function type_ctype(s, name, deftype)
	local decoder = assert(ctype_decoders[s:sub(1,1)], s)
	return decoder(s, name, deftype)
end

--convert a method type encoding to a C function type (as ctype)
local function method_ctype_string(s)
	local ret_ctype
	local arg_ctypes = {}
	for s in s:gmatch'[rnNoORV]?([^%d]+)%d+' do --eg. 'v12@0:4c8' (retval offset arg1 offset arg2 offset ...)
		local ctype = type_ctype(s, nil, not ret_ctype and 'retval')
		if not ret_ctype then
			ret_ctype = ctype
		else
			table.insert(arg_ctypes, ctype)
		end
	end
	local func_ctype = string.format('%s (*) (%s)', ret_ctype, table.concat(arg_ctypes, ', '))
	if objc.debug.methodtypes then
		log('method_ctype', '%-20s %s', s, func_ctype)
	end
	return func_ctype
end

local method_ctype = memoize(function(s) --caching to prevent ctype duplicates and avoid reparsing
	return ffi.typeof(method_ctype_string(s))
end)

--bridgesupport file parsing ---------------------------------------------------------------------------------------------

--rename table for structs to solve name clashing
local rename = {string = {}, enum = {}, typedef = {}, const = {}, ['function'] = {}}
objc.debug.rename = rename

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
	objc.load(attrs.path)
end

local typekey = ffi.abi'64bit' and 'type64' or 'type'
local valkey = ffi.abi'64bit' and 'value64' or 'value'

function tag.string_constant(attrs)
	rawset(objc, global(attrs.name, 'string'), attrs.value)
end

function tag.enum(attrs)
	if attrs.ignore == 'true' then return end
	local s = attrs[valkey] or attrs.value
	if not s then return end --value not available on this platform
	rawset(objc, global(attrs.name, 'enum'), tonumber(s))
end

local globals = {} --{name = true or ctype}; the namespace for C typedefs, consts and functions

local function cdef_node(attrs, typedecl, deftype)
	local name = global(attrs.name, typedecl)
	--note: duplicate typedef and const defs are ignored by luajit 2.0 and don't overflow its ctype table,
	--but this is an implementation detail that we shouldn't rely on, so we guard against redefinitions.
	if defined(name, globals) then return end
	local s = attrs[typekey] or attrs.type
	if not s then return end --type not available on this platform
	local ctype = type_ctype(s, name, deftype)
	cdef(name, globals, string.format('%s %s;', typedecl, ctype))
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
	if defined(name, globals) then return end
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
	local cdecl = string.format('%s %s (%s%s);', ftag.retval, ftag.name, args, vararg)
	local name = ftag.name
	ftag = nil
	if objc.debug.lazyfuncs then
		if redefined(name, globals, cdecl) then return end
		--delay cdef'ing the function until the first call, to avoid polluting the C namespace with unused declarations.
		--this is because in luajit2 can only hold as many as 64k ctypes total.
		rawset(objc, name, function(...)
			cdef(name, globals, cdecl)
			local func = C[name]
			rawset(objc, name, nil) --remove this wrapper; later calls will go to the C namespace directly.
			return func(...)
		end)
	else
		cdef(name, globals, cdecl)
	end
end

local function getsym(name) return C[name] end

function tag.function_alias(attrs) --these tags always come after the 'function' tags
	local name = attrs.name
	local original = attrs.original
	if objc.debug.func_cdef then
		local ok, func = pcall(getsym, original)
		if ok then
			rawset(objc, name, fctype)
		elseif objc.debug.errors then
			log('alias', 'symbol not found %s for %s', original, name)
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

function objc.load_bridgesuport(path, loaddeps)
	_loaddeps = loaddeps --set dep. loading flag
	local expat = require'expat' --runtime dependency: not needed with the 'notypes' flag
	expat.parse({path = path}, xml)
end

--loading frameworks -----------------------------------------------------------------------------------------------------

objc.searchpaths = {
	'/System/Library/Frameworks',
	'/Library/Frameworks',
	'~/Library/Frameworks',
}

--given a framework name or its full path, return its full path and its name
function objc.search(name)
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
		for i,path in pairs(objc.searchpaths) do
			path = string.format('%s/%s.framework', path, name)
			if canread(path) then
				return path, name
			end
		end
	end
end

objc.loaded = {} --{framework = true}
objc.loaded_bs = {} --{framework = true}

--load a framework given its name or full path; objc.load(name, false) skips loading bridgesupport files.
function objc.load(namepath, option)
	local basepath, name = objc.search(namepath)
	if not basepath then
		if objc.debug.errors then
			log('load', 'framework not found %s', namepath)
		end
		return
	end
	if not objc.loaded[basepath] then
		objc.loaded[basepath] = true
		--load the framework binary which contains classes and functions
		ffi.load(string.format('%s/%s', basepath, name), true)
		if objc.debug.load then
			log('load', '%s', basepath)
		end
		--load the bridgesupport dylib which contains callable versions of inline functions (NSMakePoint, etc.)
		local path = string.format('%s/Resources/BridgeSupport/%s.dylib', basepath, name)
		if canread(path) then
			ffi.load(path, true)
		end
	end
	if objc.debug.loadtypes and option ~= 'notypes' and not objc.loaded_bs[basepath] then
		objc.loaded_bs[basepath] = true
		--load the bridgesupport xml file which contains typedefs and constants which we can't get from the runtime.
		local path = string.format('%s/Resources/BridgeSupport/%s.bridgesupport', basepath, name)
		if canread(path) then
			objc.load_bridgesuport(path, objc.debug.loaddeps and option ~= 'nodeps')
		end
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

objc.SEL = selector

local function selector_name(sel)
    return ffi.string(ffi.C.sel_getName(sel))
end

ffi.metatype('struct objc_selector', {
	__tostring = selector_name,
})

--classes

local function class(name)
	if type(name) ~= 'string' then return name end
	return ptr(C.objc_getClass(name))
end

local function class_name(cls)
	return ffi.string(C.class_getName(cls))
end

local function superclass(cls)
	return ptr(C.class_getSuperclass(class(cls)))
end

local function metaclass(cls)
	return ptr(C.object_getClass(ffi.cast(id_ctype, class(cls))))
end

local function has_selector(cls, sel)
	return C.class_respondsToSelector(class(cls), selector(sel)) == 1
end

local function subclass(name, supername)
	check(not class(name), 'class already defined %s', tostring(name)) --OSX segfaults if trying to redefine a class
	local super = class(supername)
	check(not supername or super, 'unknown superclass %s', tostring(supername))
	local cls = check(ptr(C.objc_allocateClassPair(super, name, 0)))
   C.objc_registerClassPair(cls)
	return cls
end

function objc.class(classname, superclass, proto, ...)
	if type(superclass) == 'string' then --check for 'SuperClass <Prtocol1, Protocol2,...>' syntax
		local super, protos = superclass:match'^%s*([^%<%s]+)%s*%<%s*([^%>]+)%>%s*$'
		if super then
			local t = {}
			for proto in (protos..','):gmatch'([^,%s]+)%s*,%s*' do
				t[#t+1] = proto
			end
			return objc.class(classname, super, unpack(t))
		end
	end
	local cls = subclass(classname, superclass)
	if proto then
		objc.conforms(cls, proto, ...)
	end
	return cls
end

--protocols

local function protocol(name)
	if type(name) ~= 'string' then return name end
	return check(ptr(C.objc_getProtocol(name)), 'unknown protocol %s', name)
end

local function protocol_name(proto)
	return ffi.string(C.protocol_getName(proto))
end

function objc.conforms(cls, proto, ...) --allows inferring method types when creating methods at runtime!
	C.class_addProtocol(class(cls), protocol(proto))
	if ... then
		objc.conforms(cls, ...)
	end
end

--inst and required are optional filters: instance vs class method and required vs non-required.
local function protocol_method_type(proto, sel, inst, required)
	if inst == nil then
		return
			protocol_method_type(proto, sel, true, required) or
			protocol_method_type(proto, sel, false, required)
	end
	if required == nil then
		return
			protocol_method_type(proto, sel, inst, false) or
			protocol_method_type(proto, sel, inst, true)
	end
	local desc = C.protocol_getMethodDescription(proto, sel, required, inst)
	if desc.name == nil then return end
	return ffi.string(desc.types)
end

--find a selector in conforming protocols and if found, return its type
function conforming_method_type(cls, sel, inst)
	for _,proto in pairs(class_protocols(cls)) do
		local mtype = protocol_method_type(proto, sel, inst)
		if mtype then return mtype end
	end
end

--class/protocol properties

local function property(cls, name)
	return ptr(C.class_getProperty(cls, name))
end

local function protocol_property(proto, name, required, readonly)
	if required == nil then
		return
			protocol_property(proto, name, false, readonly) or
			protocol_property(proto, name, true, readonly)
	end
	if readonly == nil then
		return
			protocol_property(proto, name, required, true) or
			protocol_property(proto, name, required, false)
	end
	return ptr(C.protocol_getProperty(proto, name, required, readonly))
end

local function property_name(prop)
	return ffi.string(C.property_getName(prop))
end

local prop_attr_decoders = {
	G = function(s, t) t.getter = s end,
	S = function(s, t) t.setter = s end,
	R = function(s, t) t.readonly = true end,
}
local property_attributes = memoize(function(prop) --caching to prevent parsing on each property access
	local s = ffi.string(C.property_getAttributes(prop))
	local attrs = {}
	for k,v in (s..','):gmatch'(.)([^,]*),' do
		local decode = prop_attr_decoders[k]
		if decode then decode(v, attrs) end
	end
	return attrs
end)

local function property_getter(prop)
	local attrs = property_attributes(prop)
	if not attrs.getter then
		attrs.getter = property_name(prop) --default getter; cache it
	end
	return attrs.getter
end

local function property_setter(prop)
	local attrs = property_attributes(prop)
	if attrs.readonly then return end
	if not attrs.setter then
		local name = property_name(prop)
		attrs.setter = string.format('set%s%s:', name:sub(1,1):upper(), name:sub(2)) --'name' -> 'setName:'
	end
	return attrs.setter
end

--class/instance methods

local function class_method(cls, sel) --looks for inherited too
	return ptr(C.class_getClassMethod(cls, sel))
end

local function instance_method(cls, sel) --looks for inherited too
	return ptr(C.class_getInstanceMethod(cls, sel))
end

local function method(cls, sel, inst) --inst is an optional filter for instance vs class-level methods
	if inst == nil then
		return method(cls, sel, true) or method(cls, sel, false)
	end
	if inst then
		return instance_method(cls, sel)
	else
		return class_method(cls, sel)
	end
end

local function supermethod(cls, sel, inst) --last in the inheritance chain
	return method(superclass(cls), sel, inst)
end

local function method_type(method)
	return ffi.string(C.method_getTypeEncoding(method))
end

local function method_selector(method)
	return ptr(C.method_getName(method))
end

--class/instance method finding based on loose selector names.
--loose selector names are those that may or may not contain a trailing '_'.

local function find_method(cls, selname, inst)
	local sel = selector(selname)
	local meth = method(cls, sel, inst)
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

--class/instance method caller

local _instance_vars = {} --{[nptr(obj)] = {var1 = val1, ...}}

local function collect_object(obj) --note: assume that this will be called multiple times on the same obj!
	obj:release()
	_instance_vars[nptr(obj)] = nil
end

--methods for which we should refrain from retaining the result object
local noretain = {release=1, autorelease=1, retain=1, alloc=1, new=1, copy=1, mutableCopy=1}
objc.debug.noretain = noretain --publish these, maybe the user might add more?

--advanced sorcery note: ffi.gc() applies to cdata objects, not to the identities that they hold,
--so if you get the same object from two different invocations, you now have two cdata and your finalizer
--that you set with ffi.gc() will be called twice, each time when each cdata gets collected.

--so it's ok to own each new cdata that retain() returns, as long as we disown it on release().

local function method_caller(cls, selname, inst) --method caller based on a loose selector
	local sel, method = find_method(cls, selname, inst)
	if not sel then return end

	local mtype = method_type(method)
	local ctype = method_ctype(mtype)
	local imp = ffi.cast(ctype, C.method_getImplementation(method))

	local can_retain = not noretain[selname]
	local is_release = selname == 'release' or selname == 'autorelease'
	local must_disown = is_release or selname == 'dealloc'

	return function(obj, ...) --note: obj is the class for a class method
		local ok, ret = pcall(imp, ffi.cast(id_ctype, obj), sel, ...)
		if not ok then
			check(false, '[%s %s] %s\n%s', tostring(cls), tostring(sel), ret, debug.traceback())
		end
		if is_release and objc.debug.release then
			log('released', '%s, refcount after: %d', tostring(obj), tonumber(obj:retainCount()))
		end
		if must_disown then
			ffi.gc(obj, nil) --prevent calling obj:release() again on gc
		end
		if ret == nil then
			return nil --NULL -> nil
		end
		if can_retain and ffi.istype(id_ctype, ret) then
			ret = ret:retain()
			if objc.debug.retain then
				log('retained', '%s, refcount after: %d', tostring(obj), tonumber(obj:retainCount()))
			end
		end
		if not must_disown and ffi.istype(id_ctype, ret) then
			ret = ffi.gc(ret, collect_object)
		end
		return ret
	end
end

local cm_cache = {}

local class_method_caller = memoize2(function(cls, selname) --caching to prevent creating a new caller on each call
	return method_caller(cls, selname, false)
end, cm_cache)

local im_cache = {}

local instance_method_caller = memoize2(function(cls, selname) --caching to prevent creating a new caller on each call
	return method_caller(cls, selname, true)
end, im_cache)

function objc.debug.invalidate(cls) --required if overriding a method that was already called once
	cm_cache[nptr(cls)] = nil
	im_cache[nptr(cls)] = nil
end

--class/instance method override

local function add_instance_method(cls, sel, mtype, func)
	local ctype = method_ctype(mtype)
	local callback = ffi.cast(ctype, func) --classes can't be unregistered, so neither are these callback objects
	local imp = ffi.cast('IMP', callback)
	C.class_replaceMethod(cls, sel, imp, mtype) --add or replace
	objc.debug.invalidate(cls)
end

local function add_class_method(cls, sel, mtype, func)
	return add_instance_method(metaclass(cls), sel, mtype, func)
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
	local prop = property(cls, field)
	if prop then
		local getter = property_getter(prop)
		local caller = class_method_caller(cls, getter)
		if caller then --the getter is a class method so this is a "class property"
			return caller(cls)
		end
	end
	--look for a class method
	return class_method_caller(cls, field)
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
	local prop = property(cls, field)
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
	--look to override an instance method
	local sel, method = find_method(cls, field, true)
	if sel then
		local mtype = method_type(method)
		add_instance_method(cls, sel, mtype, val)
		return
	end
	--look to override a class method
	local sel, method = find_method(cls, field, false)
	if sel then
		local mtype = method_type(method)
		add_class_method(cls, sel, mtype, val)
		return
	end
	--look to override/create a conforming instance method
	local sel, mtype = find_conforming_method_type(cls, field, true)
	if sel then
		add_instance_method(cls, sel, mtype, val)
		return
	end
	--look to override/create a conforming class method
	local sel, mtype = find_conforming_method_type(cls, field, false)
	if sel then
		add_class_method(cls, sel, mtype, val)
		return
	end
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
	--look for an existing lua var
	local val = instance_var(obj, field)
	if val ~= nil then
		return val
	end
	--look for an instance property
	local prop = property(obj.isa, field)
	if prop then
		local getter = property_getter(prop)
		local caller = instance_method_caller(obj.isa, getter)
		if caller then --the getter is an instance method so this is an "instance property"
			return caller(obj)
		end
	end
	--look for an instance method
	return instance_method_caller(obj.isa, field)
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
	local prop = property(obj.isa, field)
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
	return string.format('<%s>0x%x', class_name(obj.isa), nptr(obj))
end

ffi.metatype('struct objc_object', {
	__tostring = object_tostring,
	__index = get_instance_field,
	__newindex = set_instance_field,
})

--class autoloader

setmetatable(objc, {
	__index = function(t, k)
		return class(k) or C[k]
	end,
})

--inspection -------------------------------------------------------------------------------------------------------------

--listers

local function list(listfunc, subj, t) --call a C list function f(subj, count) and return the results in a lua table
	t = t or {}
	local count = ffi.new'unsigned int[1]'
	local lst = listfunc(subj, count)
	for i = 0, count[0]-1 do
		table.insert(t, lst[i])
	end
	C.free(lst)
	return t
end

local function instance_methods(cls, methods) --returns {method1, ...}; inherited methods not included
	return list(C.class_copyMethodList, cls, methods)
end

local function class_methods(cls, methods) --returns {method1, ...}; inherited methods not included
	return instance_methods(metaclass(cls), methods)
end

local function properties(cls, props) --returns {prop1, ...}; inherited properties not included
	return list(C.class_copyPropertyList, cls, props)
end

local function protocol_properties(proto, props) --returns {prop1, ...}; inherited properties not included
	return list(C.protocol_copyPropertyList, proto, props)
end

local function protocol_methods(proto, inst, required, methods) --returns {sel, ...}; inherited methods not included
	methods = methods or {}
	if inst == nil then
		protocol_methods(proto, true, required, methods)
		protocol_methods(proto, false, required, methods)
		return methods
	end
	if required == nil then
		protocol_methods(proto, inst, false, methods)
		protocol_methods(proto, inst, true, methods)
		return methods
	end
	local count = ffi.new'unsigned int[1]'
	local desc = C.protocol_copyMethodDescriptionList(proto, required, inst, count)
	for i = 0, count[0]-1 do
		table.insert(methods, desc[i].name) --name is a SEL object
	end
	C.free(desc)
	return methods
end

local function protocols(cls)
	return list(C.class_copyProtocolList, cls)
end

local function protocol_protocols(cls)
	return list(C.protocol_copyProtocolList, cls)
end

--pretty helpers

local function p(...)
	print(string.format(...))
end

local function hr(s)
	print(s..('-'):rep(100 - #s))
end

local function header(...)
	print''
	hr'+'
	p('| '..string.format(...))
	hr'+'
end

--pretty chunks

local function inspect_properties(props)
	if #props == 0 then return end
	p('\n%-40s %-40s %-40s', 'Properties:', 'getter', 'setter')
	hr''
	for i,prop in ipairs(props) do
		local attrs = property_attributes(prop)
		p('%-40s %-40s %-40s', property_name(prop), property_getter(prop), property_setter(prop) or 'n/a')
	end
end

local function inspect_protocol_methods(proto, methods)
	if #methods == 0 then return end
	p('\n%-60s %s', 'Methods:', 'ctype')
	hr''
	for i,sel in ipairs(methods) do
		local mtype = protocol_method_type(proto, sel)
		p('%-60s %s', tostring(sel), method_ctype_string(mtype))
	end
end

local function inspect_methods(title, methods)
	if #methods == 0 then return end
	p('\n%-60s %s', title, 'ctype')
	hr''
	for i,meth in ipairs(methods) do
		local mtype = method_type(meth)
		local sel = tostring(method_selector(meth))
		p('%-60s %s', sel, method_ctype_string(mtype))
	end
end

local function superclass_names(cls)
	local t = {}
	local super = cls
	while true do
		super = superclass(super)
		if not super then break end
		t[#t+1] = tostring(super)
	end
	return #t > 0 and string.format(' (%s)', table.concat(t, ' <- ')) or ''
end

local function protocol_names(protocols)
	local t = {}
	for i = 1, #protocols do
		t[#t+1] = protocol_name(protocols[i])
	end
	return #t > 0 and string.format(' <%s>', table.concat(t, ', ')) or ''
end

--inspection api

function objc.inspect_protocol(proto)
	proto = protocol(proto)
	header('Protocol %s%s', protocol_name(proto), protocol_names(protocol_protocols(proto)))
	inspect_properties(protocol_properties(proto))
	inspect_protocol_methods(proto, protocol_methods(proto))
end

function objc.inspect_conforms(cls)
	for i,proto in ipairs(protocols(cls)) do
		--
	end
end

function objc.inspect_class(cls)
	cls = class(cls)
	header('Class %s%s%s', class_name(cls), superclass_names(cls), protocol_names(protocols(cls)))
	inspect_properties(properties(cls))
	inspect_methods('Instance Methods:', instance_methods(cls))
	inspect_methods('Class Methods:', class_methods(cls))
end


return objc
