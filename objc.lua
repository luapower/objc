--objecive-c runtime binding (Cosmin Apreutesei, public domain).
--ideas and code from TLC by Fjölnir Ásgeirsson (c) 2012, MIT license.
--tested with with LuaJIT 2.0.3, 32bit and 64bit on OSX 10.9.

local ffi = require'ffi'
local bit = require'bit'

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

struct objc_class  { Class isa; };
struct objc_object { Class isa; };

int access(const char *path, int amode); // used to check if a file exists
void free (void*); //used to free returned objc_method_description

SEL sel_registerName(const char *str);
const char* sel_getName(SEL aSelector);

Class objc_getClass(const char *name);
const char *class_getName(Class cls);

BOOL class_respondsToSelector(Class cls, SEL sel);
Method class_getClassMethod(Class aClass, SEL aSelector);
Method class_getInstanceMethod(Class aClass, SEL aSelector);
const char *method_getTypeEncoding(Method method);
IMP method_getImplementation(Method method);

Class objc_allocateClassPair(Class superclass, const char *name, size_t extraBytes);
void objc_registerClassPair(Class cls);

Protocol *objc_getProtocol(const char *name);
struct objc_method_description {
   SEL name;
   char *types;
};
struct objc_method_description protocol_getMethodDescription(Protocol *p,
	SEL aSel, BOOL isRequiredMethod, BOOL isInstanceMethod);
struct objc_method_description *protocol_copyMethodDescriptionList(
	Protocol *p, BOOL isRequiredMethod, BOOL isInstanceMethod, unsigned int *outCount);

objc_property_t class_getProperty(Class cls, const char *name);
objc_property_t protocol_getProperty(Protocol *proto, const char *name,
	BOOL isRequiredProperty, BOOL isInstanceProperty);
objc_property_t *class_copyPropertyList(Class cls, unsigned int *outCount);
objc_property_t *protocol_copyPropertyList(Protocol *proto, unsigned int *outCount);
const char *property_getName(objc_property_t property);
const char *property_getAttributes(objc_property_t property);


typedef struct objc_ivar *Ivar;
id objc_msgSend(id theReceiver, SEL theSelector, ...);

Class class_getSuperclass(Class cls);

IMP class_replaceMethod(Class cls, SEL name, IMP imp, const char *types);
BOOL class_addMethod(Class cls, SEL name, IMP imp, const char *types);

Ivar object_getInstanceVariable(id obj, const char *name, void **outValue);

unsigned method_getNumberOfArguments(Method method);
void method_getReturnType(Method method, char *dst, size_t dst_len);
void method_getArgumentType(Method method, unsigned int index, char *dst, size_t dst_len);

void method_exchangeImplementations(Method m1, Method m2);

BOOL class_addIvar(Class cls, const char *name, size_t size, uint8_t alignment, const char *types);
const char * ivar_getTypeEncoding(Ivar ivar);
ptrdiff_t ivar_getOffset(Ivar ivar);
]]

local C = ffi.C
local objc = {C = C}

objc.debug = {
	errors = true,         --log errors of any kind
	cdefs = false,         --print cdefs to stdout (then you can grab them and make your own static cdef headers)
	func_cdef = false,     --cdef functions at the time of parsing instead of lazily with a wrapper (see below)
	methods = false,       --log method ctype parsing
	load = false,          --log loaded frameworks
	release = false,       --log relases with refcount
	redef = false,         --check incompatible redefinition attempts
	stats = {
		errors = 0,         --number of cdef errors
		cdefs = 0,          --number of cdefs without error
		redef = 0,          --number of incompatible redefines (if debug.redef == true)
	}
}

--helpers

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

local function memoize2(func) --memoize a two-arg. function (:
	local memoized = memoize(function(arg1)
		return memoize(function(arg2) --each unique arg1 gets 2 closures + 1 table of overhead
			return func(arg1, arg2)
		end)
	end)
	return function(arg1, arg2)
		return memoized(arg1)(arg2)
	end
end

local function log(topic, fmt, ...) --debug logger
	io.stderr:write(string.format('[objc] %-16s %s\n', topic, string.format(fmt, ...)))
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
			error(err)
		end
		objc.debug.stats.errors = objc.debug.stats.errors + 1
		if objc.debug.errors then
			log('cdef_error', '%s\n\t%s', err, cdecl)
		end
	end
	namespace[name] = objc.debug.redef and cdecl or true --only store the cdecl if needed
	return ok
end

--type parsing/conversion to C types

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

--('{tag="f1name"f1type...}', 'name') -> 'struct tag name' or just 'tag', depending on deftype.
--('(tag="f1name"f1type...)', 'name') -> 'union tag name' or just 'tag', depending on deftype.
--note: tag is the struct tag in the C struct namespace; name is the typedef name in the C global namespace.
--named structs are recursively cdef'ed and only a tag reference is returned for them.
--for anonymous structs the complete definition is returned instead.
local function struct_ctype(s, name, deftype)

	--break the struct/union def. in its constituent parts: keyword, tag, fields
	local kw, tag, fields = s:match'^(.)([^=]*)=?(.*).$' -- '{name=fields}'
	kw = kw == '{' and 'struct' or 'union'
	if tag == '?' or tag == '' then tag = nil end -- ? or empty means anonymous struct
	if fields == '' then fields = nil end -- empty definition means opaque struct

	if not fields and not tag then return 'void'..optname(name) end --rare case: '{?}' coming from '^{?}'

	if not fields or deftype ~= 'cdef' then --opaque named struct, or asked by caller not to be cdef'ed
		return string.format('%s %s%s', kw, tag, optname(name))
	end

	if not tag or not defined(tag, structs) then --anonymous or not defined: parse it

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
function type_ctype(s, name, deftype)
	local decoder = assert(ctype_decoders[s:sub(1,1)], s)
	return decoder(s, name, deftype)
end

--convert a method type encoding to a C function type (as ctype)
local method_ctype = memoize(function(s)
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
	if objc.debug.methods then
		log('method_ctype', '%-20s %s', s, func_ctype)
	end
	return ffi.typeof(func_ctype)
end)

--bridgesupport file parsing

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

local __nodeps --switch to skip loading dependencies (set by load_bridgesuport)
function tag.depends_on(attrs)
	if __nodeps then return end
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
	if ftag.arg_depth > 1 then return end --skip child 'arg' types describing function pointers
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
	if objc.debug.func_cdef then
		cdef(name, globals, cdecl)
	else
		if redefined(name, globals, cdecl) then return end
		--delay cdef'ing the function until the first call, to avoid polluting the C namespace with unused declarations.
		--this is because in luajit2 can only hold as many as 64k ctypes total.
		rawset(objc, name, function(...)
			cdef(name, globals, cdecl)
			local func = C[name]
			rawset(objc, name, nil) --remove this wrapper; later calls will go to the C namespace directly.
			return func(...)
		end)
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
			log('alias_error', 'symbol not found %s for %s', original, name)
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

function objc.load_bridgesuport(path, nodeps)
	__nodeps = nodeps --set nodeps barrier
	local expat = require'expat' --runtime dependency: not needed with the 'notypes' flag
	expat.parse({path = path}, xml)
end

--loading frameworks

objc.searchpaths = {
	'/System/Library/Frameworks',
	'/Library/Frameworks',
	'~/Library/Frameworks',
}

--given a framework name or its full path, return its full path and its name
local function search(name)
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
	local basepath, name = search(namepath)
	if not basepath then
		if objc.debug.errors then
			log('load_error', 'framework not found %s', namepath)
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
	if option ~= 'notypes' and not objc.loaded_bs[basepath] then
		objc.loaded_bs[basepath] = true
		--load the bridgesupport xml file which contains typedefs and constants which we can't get from the runtime.
		local path = string.format('%s/Resources/BridgeSupport/%s.bridgesupport', basepath, name)
		if canread(path) then
			objc.load_bridgesuport(path, option == 'nodeps')
		end
	end
end

--selectors

local selector_object = memoize(function(sel)
	--replace '_' with ':' except at the beginning
	sel = sel:match('^_*') .. sel:gsub('^_*', ''):gsub('_', ':')
	return ptr(C.sel_registerName(sel))
end)

local function selector(sel)
	if type(sel) ~= 'string' then return sel end
	return selector_object(sel)
end

objc.SEL = selector

local function selector_name(sel)
    return ffi.string(ffi.C.sel_getName(sel))
end

ffi.metatype('struct objc_selector', {
	__tostring = selector_name,
})

--classes & protocols

local function class_name(class)
	return ffi.string(C.class_getName(class))
end

local function class(cls)
	if type(cls) ~= 'string' then return cls end
	return ptr(C.objc_getClass(cls))
end

local function has_selector(cls, sel)
	return C.class_respondsToSelector(class(cls), selector(sel)) == 1
end

--protocols

local function protocol(proto)
	if type(proto) ~= 'string' then return proto end
	return ptr(C.objc_getProtocol(proto))
end

--protocol methods

local function protocol_method_type(proto, sel)
	local                 desc = C.protocol_getMethodDescription(proto, sel, false, true)
	if not desc.name then desc = C.protocol_getMethodDescription(proto, sel, true,  true) end
	if not desc.name then desc = C.protocol_getMethodDescription(proto, sel, false, false) end
	if not desc.name then desc = C.protocol_getMethodDescription(proto, sel, true,  false) end
	if not desc.name then return end
	return ffi.string(desc.types)
end

local function protocol_methods(proto) --for introspection
	local methods = {}
	local function add_methods(required, instance)
		local count = ffi.new'unsigned int[1]'
		local desc = C.protocol_copyMethodDescriptionList(proto, required, instance, count)
		for i = 0, count[0]-1 do
			local sel = desc[i].name
			local mtype = ffi.string(desc[i].types)
			methods[nptr(sel)] = mtype
		end
		C.free(desc)
	end
	add_methods(false, true)
	add_methods(true,  true)
	add_methods(false, false)
	add_methods(true,  false)
	return methods
end)

--class conforming to protocols

local class_protocols = memoize(function(class)
	return {}
end)

function objc.conforms(cls, proto, ...)
	proto = protocol(proto)
	cls = class(cls)
	class_protocols(cls)[nptr(proto)] = proto
	if ... then
		objc.conforms(cls, ...)
	end
end

function class_method_type(cls, sel) --search conforming protocols for a selector and return its type
	for _,proto in pairs(class_protocols(cls)) do
		local mtype = protocol_method_type(proto, sel)
		if mtype then return mtype end
	end
end

--class/protocol properties

local function class_property(cls, name)
	return ptr(C.class_getProperty(class(cls), name))
end

local function protocol_property(proto, name, required, readonly)
	return ptr(C.protocol_getProperty(proto, name, required, readonly))
end

local decoders = {
	G = function(s, t) t.getter = s end,
	S = function(s, t) t.setter = s end,
	R = function(s, t) t.readonly = true end,
}
local property_attributes = memoize(function(prop)
	local s = ffi.string(C.property_getAttributes(prop))
	local attrs = {}
	for k,v in (s..','):gmatch'(.)([^,]*),' do
		local decode = decoders[k]
		if decode then decode(v, attrs) end
	end
	return attrs
end)

local function properties(get_plist, cls) --for introspection
	local props = {}
	local count = ffi.new'unsigned int[1]'
	local plist = get_plist(cls, count)
	for i = 0, count[0]-1 do
		props[property_name(plist[i])] = property_attributes(plist[i])
	end
	C.free(plist)
	return props
end

local function class_properties(cls) --for introspection
	return properties(C.class_copyPropertyList, cls)
end

local function protocol_properties(proto) --for introspection
	return properties(C.protocol_copyPropertyList, proto)
end

--class/instance method caller

local function get_sel_and_method(get_method, class, sel_str)

	local sel = selector(sel_str)
	local method = ptr(get_method(class, sel))

	--method not found, try again with a trailing '_'
	if not method and not sel_str:find'[_%:]$' then
		return get_sel_and_method(get_method, class, sel_str .. '_')
	end

	return sel, method
end

local id_ctype = ffi.typeof'id'

local _instance_vars = {} --{[nptr(obj)] = {var1 = val1, ...}}

local function collect_object(obj)
	obj:release()
	_instance_vars[nptr(obj)] = nil --variables stay after obj:release() until the object is gc'ed
end

local function method_caller(get_method, class, sel_str)
	local sel, method = get_sel_and_method(get_method, class, sel_str)
	if not method then return end

	local mtype = ffi.string(C.method_getTypeEncoding(method))
	local ctype = method_ctype(mtype)
	local imp = ffi.cast(ctype, C.method_getImplementation(method))

	local isalloc = sel_str:find'^alloc'
	local isnew = sel_str == 'new'
	local isrelease = sel_str == 'release'

	return function(obj, ...)
		local ok, ret = pcall(imp, ffi.cast(id_ctype, obj), sel, ...)
		if not ok then
			error(string.format('[%s %s] %s\n%s', tostring(class), tostring(sel), ret, debug.traceback()))
		end
		if ret == nil then return nil end --NULL objects -> nil
		if (isalloc or isnew) and ffi.istype(id_ctype, ret) then
			ret = ret:retain()
			if isalloc then
				ret = ffi.gc(ret, collect_object)
			end
		elseif isrelease then
			ffi.gc(obj, nil) --prevent calling obj:release() again on gc
			if objc.debug.release then
				log('release_object', '%s, refcount after: %d', tostring(obj), tonumber(obj:retainCount()))
			end
		end
		return ret
	end
end

local get_class_method = memoize2(function(cls, sel_str)
	return method_caller(C.class_getClassMethod, cls, sel_str)
end)

local get_instance_method = memoize2(function(obj, sel_str)
	return method_caller(C.class_getInstanceMethod, obj.isa, sel_str)
end)

--get/set class/instance lua vars

local _class_vars = {} --{[nptr(cls)] = {var1 = val1, ...}}

local function get_var(namespace)
	return function(obj, var)
		local vars = namespace[obj]
		return vars and vars[var]
	end
end
local get_class_var    = get_var(_class_vars)
local get_instance_var = get_var(_instance_vars)

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

--get/set class fields

local function get_class_field(class, field)
	assert(class ~= nil, 'attempt to index a NULL class object')
	local value = class_variables(class)[field]
	if value ~= nil then
		return value
	end
	return class_method(class, field)
end

local function set_class_method(cls, sel_str, func)

	--TODO: check class_method_type(cls, sel_str) and add the method


--[[
	local sel, method = get_sel_and_method(C.class_getClassMethod, class, sel_str)

	if not sel then --new method, needs type

	 typeEncoding = typeEncoding or "v@:"
    local signature = objc.impSignatureForTypeEncoding(typeEncoding)
    local imp = ffi.cast(signature, lambda)
    imp = ffi.cast("IMP", imp)

    -- If one exists, the existing/super method will be renamed to this selector
    local renamedSel = objc.SEL("__"..objc.selToStr(selector))

    local couldAddMethod = C.class_addMethod(class, selector, imp, typeEncoding)
    if couldAddMethod == 0 then
        -- If the method already exists, we just add the new method as old{selector} and swizzle them
        if C.class_addMethod(class, renamedSel, imp, typeEncoding) == 1 then
            objc.swizzle(class, selector, renamedSel)
        else
            error("Couldn't replace method")
        end
    else
        local superClass = C.class_getSuperclass(class)
        local superMethod = C.class_getInstanceMethod(superClass, selector)
        if superMethod ~= nil then
            C.class_addMethod(class, renamedSel, C.method_getImplementation(superMethod), C.method_getTypeEncoding(superMethod))
        end
    end
	]]
end

local function set_class_field(class, field, value)
	assert(class ~= nil, 'attempt to index a NULL class object')
	local value = class_variables(class)[field]
	if value ~= nil then
		class_variables(class)[field] = value
	end

	if type(value) == 'function' then
		set_class_method(class, field, value)
	else
		class_variables(class)[field] = value
	end
end

ffi.metatype('struct objc_class', {
	__tostring = class_name,
	__index = get_class_field,
	__newindex = set_class_field,
})

--instance methods & variables

local function get_instance_field(obj, field) --try get_selectors, instance variables, and methods, in order
	assert(obj ~= nil, 'attempt to index a NULL object')
	local get_sel = instance_get_selectors(obj.isa)(field)
	if get_sel then
		return obj[get_sel](obj, field)
	end
	local value = instance_variables(obj)[field]
	if value ~= nil then
		return value
	end
	return instance_methods(obj.isa)(field)
end

local function set_instance_field(obj, field, value)
	assert(obj ~= nil, 'attempt to index a NULL object')
	local value = instance_variables(obj)[field]
	if value ~= nil then
		instance_variables(obj)[field] = value
	else
		local set_sel = instance_set_selectors(obj.isa)(field)
		if set_sel then
			obj[set_sel](obj, field, value)
		else
			instance_variables(obj)[field] = value
		end
	end
end

local function object_description(obj)
	if obj == nil then return 'nil' end
	return ffi.string(obj:description():UTF8String())
end

ffi.metatype('struct objc_object', {
	__tostring = object_description,
	__index = get_instance_field,
	__newindex = set_instance_field,
})

--class autoloader

setmetatable(objc, {
	__index = function(t, k)
		return class(k) or C[k]
	end,
})

--class creation & extension

function objc.class(classname, superclass, proto, ...)
	assert(not class(classname), 'class already defined') --OSX segfaults if trying to redefine a class
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
	superclass = class(superclass)
	local cls = ptr(C.objc_allocateClassPair(superclass, classname, 0))
   C.objc_registerClassPair(cls)
	if proto then
		objc.conforms(cls, proto, ...)
	end
	return cls
end

--load the runtime

if ffi.arch ~= 'arm' and ffi.os == 'OSX' then
	ffi.load('libobjc.A.dylib', true)
end

return objc
