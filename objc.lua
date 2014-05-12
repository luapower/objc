--objecive-c runtime binding (Cosmin Apreutesei, public domain).
--ideas and code from TLC by Fjölnir Ásgeirsson (c) 2012, MIT license.
--tested with with LuaJIT 2.0.3, 32bit and 64bit on OSX 10.9.

local ffi = require'ffi'
local bit = require'bit'
local expat = require'expat'

--assert(ffi.os == 'OSX', 'platform not OSX')

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

objc.debug = false --set to 'cdef' for printing out the cdefs too

--helpers

local function ptr(p) --convert NULL pointer to nil for easier handling (say 'not ptr' instead of 'ptr == nil')
	if p == nil then return nil end
	return p
end

local function nptr(p) --convert pointer to lua number for using as table key
	if p == nil then return nil end
	return tonumber(ffi.cast('intptr_t', p))
end

local function memoize(func) --memoize function that can memoize pointer arguments too
	local cache = {}
	return function(input)
		local key = input
		if type(key) == 'cdata' then
			key = nptr(key)
		end
		local ret = cache[key]
		if not ret then
			ret = func(input)
			cache[key] = ret
		end
		return ret
	end
end

local function log(topic, fmt, ...) --to be called only when objc.debug is set
	io.stderr:write(string.format('[objc] %-20s %s\n', topic, string.format(fmt, ...)))
end

local function cdef(s) --load a cdef via ffi (set objc.debug = 'cdef' to also print those out)
	local ok, err = pcall(ffi.cdef, s)
	if not objc.debug then return end
	if ok then
		if objc.debug == 'cdef' then
			print(s)
		end
	elseif not err:find'attempt to redefine' then
		log('cdef-error', '%s\n\t%s', err, s)
	end
end

local function canread(path) --check that a file is readable without having to open it
	return C.access(path, bit.lshift(1,2)) == 0
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

--('{sname="f1name"f1type...}', 'name') -> 'struct sname name' or just 'sname', depending on deftype.
--('(uname="f1name"f1type...)', 'name') -> 'union uname name' or just 'uname', depending on deftype.
--note: sname is the struct name in the C struct namespace; name is the typedef name in the C global namespace.
--named structs are recursively cdef'ed and only a reference from the struct namespace is returned for them.
--for anonymous structs the complete definition is returned instead.
local function struct_ctype(s, name, deftype)

	--break the struct/union def. in its constituent parts: keyword, name, type-string
	local kw, s = s:match'^(.)(.*).$' --'{...}' or '(...)'
	kw = kw == '{' and 'struct' or 'union'
	local sname, fields = s:match'^([^=]*)=?(.*)$' -- 'name=fields'
	if sname == '?' or sname == '' then sname = nil end -- ? or empty means anonymous struct/union
	if fields == '' then fields = nil end -- empty definition means opaque struct/union

	if not fields or deftype ~= 'sref' then
		--opaque struct, either because it has no definition (no fields), or because it comes
		--from bridgesupport tags `constant`, `opaque`, `function`, or from a method type string,
		--which should've been already defined from loading the bridgesupport file of the framework.
		--assert(sname) --must be a named struct in this case.
		return string.format('%s %s%s', kw, sname, optname(name))
	end

	--parse the fields which come as '"name1"type1"name2"type2...'
	local t = {}
	local function addfield(fname, s)
		if fname == '' then fname = nil end --empty field name means unnamed struct/union (different from anonymous)
		table.insert(t, type_ctype(s, fname, 'sref')) --eg. 'struct _NSPoint origin'
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
	local ctype = string.format('%s%s {\n\t%s;\n}', kw, optname(sname), table.concat(t, ';\n\t'))

	if not sname then
		--anonymous struct/union: return the full definition
		return string.format('%s%s', ctype, optname(name))
	else
		--named struct: cdef it and return as struct name reference (sref) instead
		cdef(ctype .. ';')
		return string.format('%s %s%s', kw, sname, optname(name))
	end
end

local function bitfield_ctype(s, name) --('bN', 'name') -> 'unsigned int name: N'
	local n = s:match'^b(%d+)$'
	return string.format('unsigned int %s: %d', name, n)
end

local function pointer_ctype(s, name, ...) --('^type', 'name') -> ctype('type', '(*name)')
	return type_ctype(s:sub(2), string.format('(*%s)', name or ''), ...)
end

local function char_ptr_ctype(s, name) --('*', 'name') -> 'char (*name)'
	return string.format('char *%s', name or '')
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
--deftype specifies how structs should be parsed and returned:
--  'sref' means cdef the structs and then reference them as 'struct foo'
--  nil (default) means don't cdef structs.
function type_ctype(s, name, deftype)
	local decoder = ctype_decoders[s:sub(1,1)]
	return decoder(s, name, deftype)
end

--convert a method type encoding to a C function type (as ctype)
local method_ctype = memoize(function(s)
	local ret_ctype
	local arg_ctypes = {}
	for s in s:gmatch'[rnNoORV]?([^%d]+)%d+' do --result-type offset arg1-type offset arg2-type offset ...
		local ctype = type_ctype(s)
		if not ret_ctype then
			ret_ctype = ctype
		else
			table.insert(arg_ctypes, ctype)
		end
	end
	local func_ctype = string.format('%s (*) (%s)', ret_ctype, table.concat(arg_ctypes, ', '))
	if objc.debug then
		log('method_ctype', '%-20s %s', s, func_ctype)
	end
	return ffi.typeof(func_ctype)
end)

--bridgesupport file parsing

local xml    = {} --{expat_callback = handler}
local tag    = {} --{tag = start_tag_handler}
local endtag = {} --{tag = end_tag_handler}

function tag.depends_on(attrs)
	objc.load(attrs.path)
end

function tag.string_constant(attrs)
	objc[attrs.name] = attrs.value
end

function tag.enum(attrs)
	objc[attrs.name] = tonumber(attrs.value)
end

local typekey = ffi.abi'64bit' and 'type64' or 'type'

local function cdef_node(attrs, typedecl, deftype)
	local s = attrs[typekey] or attrs.type
	if not s then return end --type not available on this platform
	local ctype = type_ctype(s, attrs.name, deftype)
	cdef(string.format('%s %s;', typedecl, ctype))
end

function tag.constant(attrs)
	cdef_node(attrs, 'const')
end

function tag.struct(attrs)
	cdef_node(attrs, 'typedef', 'sref')
end

function tag.cftype(attrs)
	cdef_node(attrs, 'typedef', 'sref')
end

function tag.opaque(attrs)
	cdef_node(attrs, 'typedef')
end

local ftag --current function tag object

tag['function'] = function(attrs)
	ftag = {name = attrs.name, retval = type_ctype'v', args = {}}
end

function tag.retval(attrs)
	if not ftag then return end
	ftag.retval = type_ctype(attrs[typekey] or attrs.type)
end

function tag.arg(attrs)
	if not ftag then return end
	local s = attrs[typekey] or attrs.type
	if s == '@?' then s = '@' end --block definition
	table.insert(ftag.args, type_ctype(s))
end

endtag['function'] = function()
	cdef(string.format('%s %s (%s);', ftag.retval, ftag.name, table.concat(ftag.args, ', ')))
	ftag = nil
end

function xml.start_tag(name, attrs)
	if tag[name] then tag[name](attrs) end
end

function xml.end_tag(name)
	if endtag[name] then endtag[name]() end
end

function objc.load_bridgesuport(path)
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
function objc.load(namepath, bridgesupport)
	local basepath, name = search(namepath)
	if not basepath then
		error(string.format('[objc] %s not found', namepath))
	end
	if bridgesupport ~= false and not objc.loaded_bs[basepath] then
		objc.loaded_bs[basepath] = true
		--bridgesupport files contain typedefs and constants which we can't get from the runtime
		local path = string.format('%s/Resources/BridgeSupport/%s.bridgesupport', basepath, name)
		if canread(path) then
			objc.load_bridgesuport(path)
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
		if objc.debug then
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
end)

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
end)

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
		print(string.format('method not found [%s %s]', tostring(class), sel_str))
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
	if objc.debug then
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
		local v = class(k) or C[k]
		rawset(t, k, v) --TODO: double-caching of class objects
		return v
	end,
})

objc.NSWin = objc.class(objc.NSWindow)

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
