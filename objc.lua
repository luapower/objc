--objecive-c runtime binding (Cosmin Apreutesei, public domain).
--ideas and code from TLC by Fjölnir Ásgeirsson (c) 2012, MIT license.
--tested with with LuaJIT 2.0.3, 32bit and 64bit on OSX 10.9.

local ffi = require'ffi'
local bit = require'bit'
local expat = require'expat'

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

struct objc_class  { Class isa; };
struct objc_object { Class isa; };

int access(const char *path, int amode); // used to check if a file exists
void free (void*); //used to free returned objc_method_description

SEL sel_registerName(const char *str);
const char* sel_getName(SEL aSelector);

Class objc_getClass(const char *name);
const char *class_getName(Class cls);

Method class_getClassMethod(Class aClass, SEL aSelector);
Method class_getInstanceMethod(Class aClass, SEL aSelector);

IMP method_getImplementation(Method method);
const char *method_getTypeEncoding(Method method);

Class objc_allocateClassPair(Class superclass, const char *name, size_t extraBytes);
void objc_registerClassPair(Class cls);

Protocol *objc_getProtocol(const char *name);

struct objc_method_description {
   SEL name;
   char *types;
};

struct objc_method_description *protocol_copyMethodDescriptionList(
	Protocol *p,
	BOOL isRequiredMethod, BOOL isInstanceMethod,
	unsigned int *outCount);



typedef struct objc_ivar *Ivar;
id objc_msgSend(id theReceiver, SEL theSelector, ...);

BOOL   class_respondsToSelector(Class cls, SEL sel);
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
	errors = true,         --log errors
	cdefs = false,         --print cdefs to stdout
	func_cdef = false,     --cdef functions at the time of parsing instead of lazily (see below)
	methods = false,       --log method ctypes
	load = false,          --log framework paths
	release = false,       --log relases with refcount
	redef = false,         --check inconsistent struct redefinition attempts
	stats = {
		errors = 0,         --number of cdef errors
		cdefs = 0,          --number of cdefs
		redef = 0,          --number of struct redefines (if debug.redef == true)
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

local function memoize(func, cache) --memoize function that can memoize pointer arguments too
	cache = cache or {}
	return function(input)
		local key = input
		if type(key) == 'cdata' then
			key = nptr(key)
		end
		local ret = rawget(cache, key)
		if ret == nil then
			ret = func(input)
			if ret == nil then return end
			rawset(cache, key, ret)
		end
		return ret
	end
end

local function log(topic, fmt, ...) --debug logger
	io.stderr:write(string.format('[objc] %-20s %s\n', topic, string.format(fmt, ...)))
end

local function canread(path) --check that a file is readable without having to open it
	return C.access(path, bit.lshift(1,2)) == 0
end

local function defined(name, namespace) --check if a name is already defined in a namespace
	return namespace[name] and not objc.debug.redef
end

local function check_redef(name, old_cdecl, new_cdecl) --check and report cdecl redefinitions
	if not objc.debug.redef  then return end
	if old_cdecl == new_cdecl then return end
	objc.debug.stats.redef = objc.debug.stats.redef + 1
	objc.debug.stats.errors = objc.debug.stats.errors + 1
	if not objc.debug.errors then return end
	log('redefinition', '%s\n~~old:\n%s\n~~new:\n%s', name, old_cdecl, new_cdecl)
end

local function cdef(name, namespace, cdecl) --define a C type, const or function via ffi.cdef
	local old_cdecl = namespace[name]
	if old_cdecl then
		check_redef(name, old_cdecl, cdecl)
		return
	end
	local ok, err = pcall(ffi.cdef, cdecl)
	if ok then
		objc.debug.stats.cdefs = objc.debug.stats.cdefs + 1
		if objc.debug.cdefs then
			print(s)
		end
	else
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
	local kw, s = s:match'^(.)(.*).$' --'{...}' or '(...)'
	kw = kw == '{' and 'struct' or 'union'
	local tag, fields = s:match'^([^=]*)=?(.*)$' -- 'name=fields'
	if tag == '?' or tag == '' then tag = nil end -- ? or empty means anonymous struct/union
	if fields == '' then fields = nil end -- empty definition means opaque struct/union

	if not fields or deftype ~= 'cdef' then --opaque struct or asked by caller not to be cdef'ed
		return string.format('%s %s%s', kw, tag or '_', optname(name))
	end

	if not tag or not defined(tag, structs) then --not defined or anonymous: parse it

		--parse the fields which come as '"name1"type1"name2"type2...'
		local t = {}
		local function addfield(fname, s)
			if fname == '' then fname = nil end --empty field name means unnamed struct/union (different from anonymous)
			table.insert(t, type_ctype(s, fname, 'cdef')) --eg. 'struct _NSPoint origin'
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

		--anonymous struct/union: return the full definition
		if not tag then
			return string.format('%s%s', ctype, optname(name))
		end

		--named struct: cdef it
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
		name = string.format('(*%s)', name or '') --make `(*int)[8]` instead of the ambiguous `*int[8]`
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
	['Z'] = primitive_ctype'bool', --undocumented

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
	local decoder = ctype_decoders[s:sub(1,1)]
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

function tag.string_constant(attrs)
	rawset(objc, global(attrs.name, 'string'), attrs.value)
end

function tag.enum(attrs)
	rawset(objc, global(attrs.name, 'enum'), tonumber(attrs.value))
end

local typekey = ffi.abi'64bit' and 'type64' or 'type'

local globals = {} --{name = true or ctype}; the namespace for C typedefs, consts and functions

local function cdef_node(attrs, typedecl, deftype)
	local name = global(attrs.name, typedecl)
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
	cdef_node(attrs, 'typedef', 'cdef')
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
	if defined(name, globals) then return end
	ftag = {name = name, retval = type_ctype'v', args = {}}
end

function tag.retval(attrs)
	if not ftag then return end --canceled by prev. tag.arg
	local s = attrs[typekey] or attrs.type
	if not s then ftag = nil; return end --arg not available on 32bit, cancel the recording
	ftag.retval = type_ctype(s, nil, 'retval')
end

function tag.arg(attrs)
	if not ftag then return end --canceled by prev. tag.arg or tag.retval
	local s = attrs[typekey] or attrs.type
	if s == '@?' then s = '@' end --block definition
	if not s then ftag = nil; return end --arg not available on 32bit, skip the entire function
	table.insert(ftag.args, type_ctype(s))
end

endtag['function'] = function()
	if not ftag then return end --canceled by prev. tag.arg or tag.retval
	local cdecl = string.format('%s %s (%s);', ftag.retval, ftag.name, table.concat(ftag.args, ', '))
	local fname = ftag.name
	ftag = nil
	if objc.debug.func_cdef then
		cdef(fname, globals, cdecl)
	else
		local old_cdecl = globals[fname]
		if old_cdecl then
			check_redef(fname, old_cdecl, cdecl)
			return
		end
		--delay the cdef'ing of the function until the first call, to avoid polluting the C space with unused declarations.
		--this is because in luajit2 can only hold as many as 64k ctypes total.
		rawset(objc, fname, function(...)
			cdef(fname, globals, cdecl)
			local fctype = C[fname]
			rawset(objc, fname, nil) --remove this wrapper; later calls will go to the C namespace directly.
			return fctype(...)
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
	expat.parse({path = path}, xml)
end

--loading frameworks

objc.searchpaths = {
	'/System/Library/Frameworks/%s.framework',
	'/Library/Frameworks/%s.framework',
	'~/Library/Frameworks/%s.framework',
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
		for i,fmt in pairs(objc.searchpaths) do
			local path = fmt:format(name)
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
		error(string.format('[objc] %s not found', namepath))
	end
	if option ~= 'notypes' and not objc.loaded_bs[basepath] then
		objc.loaded_bs[basepath] = true
		--bridgesupport files contain typedefs and constants which we can't get from the runtime
		local path = string.format('%s/Resources/BridgeSupport/%s.bridgesupport', basepath, name)
		if canread(path) then
			objc.load_bridgesuport(path, option == 'nodeps')
		end
		--bridgesupport dylibs contain callable versions of inline functions (NSMakePoint, etc.)
		local path = string.format('%s/Resources/BridgeSupport/%s.dylib', basepath, name)
		if canread(path) then
			ffi.load(path, true)
		end
	end

	if not objc.loaded[basepath] then
		objc.loaded[basepath] = true
		ffi.load(string.format('%s/%s', basepath, name), true)
		if objc.debug.load then
			log('load', '%s', basepath)
		end
	end
end

--selectors

local selector = memoize(function(sel)
	--replace '_' with ':' except at the beginning
	sel = sel:match('^_*') .. sel:gsub('^_*', ''):gsub('_', ':')
	return ptr(C.sel_registerName(sel))
end)

local function selector_name(sel)
    return ffi.string(ffi.C.sel_getName(sel))
end

ffi.metatype('struct objc_selector', {
	__tostring = selector_name,
})

--classes

local class_object = memoize(function(name)
	return ptr(C.objc_getClass(name))
end, objc)

local class_name = memoize(function(class)
	return ffi.string(C.class_getName(class))
end)

local function class(cls)
	if type(cls) ~= 'string' then return cls end
	return class_object(cls)
end

--protocols

local protocol_object = memoize(function(name)
	return ptr(C.objc_getProtocol(name))
end, objc)

local function protocol(proto)
	if type(proto) ~= 'string' then return proto end
	return protocol_object(proto)
end

local protocol_methods = memoize(function(proto)
	local methods = {}
	local function add_methods(required, instance)
		local count = ffi.new'unsigned int[1]'
		local desc = C.protocol_copyMethodDescriptionList(proto, required, instance, count)
		for i = 0, count[0]-1 do
			local sel = desc[i].name
			local types = ffi.string(desc[i].types)
			--print(selector_name(sel), types)
			table.insert(methods, {sel = sel, types = types})
		end
		C.free(desc)
	end
	add_methods(true, true)
	add_methods(true, false)
	add_methods(false, true)
	add_methods(false, false)
	return methods
end)

local class_protocols = memoize(function(class)
	return {}
end)

function objc.conforms(cls, proto)
	proto = protocol(proto)
	cls = class(cls)
	class_protocols(cls)[nptr(proto)] = protocol_methods(proto)
end

--methods

local id_ctype = ffi.typeof'id'

local function get_sel_and_method(get_method, class, sel_str)

	local sel = selector(sel_str)
	local method = ptr(get_method(class, sel))

	--method not found, try again with a trailing '_'
	if not method and not sel_str:find'[_%:]$' then
		return get_sel_and_method(get_method, class, sel_str .. '_')
	end

	return sel, method
end

local release_object --fw. decl.

local function create_method_wrapper(get_method, class, sel_str)

	local sel, method = get_sel_and_method(get_method, class, sel_str)

	if not method then
		if objc.debug.errors then
			log('method_call', 'method not found [%s %s]', tostring(class), sel_str)
		end
		return nil
	end

	local mtype = ffi.string(C.method_getTypeEncoding(method))
	local ctype = method_ctype(mtype)
	local imp = ffi.cast(ctype, C.method_getImplementation(method))

	local isalloc = sel_str:find'^alloc'
	local isnew = sel_str == 'new'

	return function(self, ...)

		local ok, ret = pcall(imp, ffi.cast(id_ctype, self), sel, ...)

		if not ok then
			error(string.format('[objc] %s\n%s', ret, debug.traceback()))
		end

		if ret ~= nil and ffi.istype(id_ctype, ret) then
			if isalloc or isnew then
				ret = ret:retain()
			end
			if isalloc then
				ret = ffi.gc(ret, release_object)
			end
		end

		return ret
	end
end

--class methods & variables

local class_methods = memoize(function(class)
	return memoize(function(sel_str)
		return create_method_wrapper(C.class_getClassMethod, class, sel_str)
	end)
end)

local class_variables = memoize(function(class)
	return {}
end)

local function get_class_field(class, field)
	assert(class ~= nil, 'attempt to index a NULL class object')
	local value = class_variables(class)[field]
	if value ~= nil then return value end
	return class_methods(class)(field)
end

local function set_class_method(class, sel_str, func)

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

local _instance_variables = {} --{[nptr(obj)] = {var1 = val1, ...}}

function release_object(obj)
	if objc.debug.release then
		log('release_object', '%s, refcount: %d', tostring(obj), tonumber(obj:retainCount()))
	end
	obj:release()
	_instance_variables[nptr(obj)] = nil
end

local instance_methods = memoize(function(class)
	return memoize(function(sel_str)
		return create_method_wrapper(C.class_getInstanceMethod, class, sel_str)
	end)
end)

local function instance_variables(obj)
	local nobj = nptr(obj)
	local vars = _instance_variables[nobj]
	if not vars then
		vars = {}
		_instance_variables[nobj] = vars
	end
	return vars
end

local function get_instance_field(obj, field)
	assert(obj ~= nil, 'attempt to index a NULL object')
	local value = instance_variables(obj)[field]
	if value ~= nil then return value end
	return instance_methods(obj.isa)(field)
end

local function set_instance_field(obj, field, value)
	assert(obj ~= nil, 'attempt to index a NULL object')
	instance_variables(obj)[field] = value
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
		return class_object(k) or C[k]
	end,
})

--class creation & extension

function objc.class(classname, superclass)
	superclass = class(superclass)
	local class = ptr(C.objc_allocateClassPair(superclass, classname, 0))
   C.objc_registerClassPair(class)
	return class
end

--load the runtime and base frameworks

if ffi.arch ~= 'arm' and ffi.os == 'OSX' then
	ffi.load('libobjc.A.dylib', true)
	objc.load'CoreFoundation'
	objc.load'Foundation'
	objc.load'Carbon'
end


if not ... then require'objc_test' end

return objc
