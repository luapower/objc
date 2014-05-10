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

struct objc_method_description *protocol_copyMethodDescriptionList(Protocol *p,
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

local function ptr(p) --convert NULL pointer to nil for easier handling (say `not ptr` instead of `ptr == nil`)
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
	elseif not err:match'attempt to redefine' then
		log('cdef-error', '%s\n\t%s', err, s)
	end
end

local function canread(path) --check that a file is readable without having to open it
	return C.access(path, bit.lshift(1,2)) == 0
end

--type parsing/conversion to C types

local type_ctype --fw. decl.

local function optname(name) --format an optional name: if given, return it with a space in front
	return name and ' '..name or ''
end

local function array_ctype(s, name) --`[ntype]` -> `type name[n]`
	local n,s = s:match'^.(%d+)(.-)%]$'
	return string.format('%s%s[%d]', type_ctype(s, nil, 'sref'), optname(name), n)
end

--`(uname=type)` -> `uname` or `union uname` depending on deftype
--`{sname=type}` -> `sname` or `struct sname` depending on deftype
--`{_NSRect="origin"{_NSPoint=...}...}` -> `struct _NSRect {struct _NSPoint origin; ...};`
local function struct_ctype(s, name, deftype)

	print(s)

	--break the struct def. in its constituent parts
	local kw, sname, fields = s:match'^(.)([^%=]+)%=(.-).$' -- `{name=type}`
	if sname == '?' then sname = nil end -- ? means anonymous
	if fields == '' then fields = nil end -- empty fields means opaque
	if not kw then -- no equal sign means opaque, eg. `{OpaqueAEDataStorageType}`
		kw, sname = s:match'^(.)(.-).$'
	end
	kw = kw == '{' and 'struct' or 'union' -- structs are `{...}`, unions are `(...)`

	if not deftype then --don't want the def., only the name
		return sname
	end

	if not fields or deftype == 'sref-nodef' then --opaque or wanted as such (can't be anonymous)
		return string.format('%s %s%s', kw, sname, optname(name))
	end

	--accumulate field type-name pairs
	local t = {}
	local function addfield(name, stype)
		table.insert(t, type_ctype(stype, name, 'sref')) --results in `struct _NSPoint origin`
		return '' --remove the match
	end
	local s = fields
	local n
	while s ~= '' do
		s,n = s:gsub('^"([^"]+)"(%b{})', addfield) --try "field"{...}
		if n == 0 then
			s,n = s:gsub('^"([^"]+)"(%b())', addfield) --try "field"(...)
		end
		if n == 0 then
			s,n = s:gsub('^"([^"]+)"(%b[])', addfield) --try "field"[...]
		end
		if n == 0 then
			s,n = s:gsub('^"([^"]+)"([^"]+)', addfield) --try "field"...
		end
		assert(n > 0, s)
	end
	local ctype = string.format('%s%s {\n\t%s;\n}', kw, optname(sname), table.concat(t, ';\n\t'))

	if not sname then --anonymous, return its ctype as is
		return ctype
	end

	--named struct, cdef it and return its struct-namespace name instead
	cdef(ctype .. ';')
	return string.format('%s %s%s', kw, sname, optname(name))
end

local function bitfield_ctype(s, name) --`bN` -> 'unsigned int name: N`
	local n = s:match'^b(%d+)$'
	return string.format('unsigned int %s: %d', name, n)
end

local function pointer_ctype(s, name, deftype) --`^type` -> `type* name`
	return string.format('%s*%s', type_ctype(s:sub(2), nil, deftype), optname(name))
end

local ctype_decoders = {
	['c'] = 'char',
	['i'] = 'int',
	['s'] = 'short',
	['l'] = 'long', --treated as a 32-bit quantity on 64-bit programs
	['q'] = 'long long',

	['C'] = 'unsigned char',
	['I'] = 'unsigned int',
	['S'] = 'unsigned short',
	['L'] = 'unsigned long',
	['Q'] = 'unsigned long long',

	['f'] = 'float',
	['d'] = 'double',

	['B'] = 'BOOL',
	['v'] = 'void',
	['*'] = 'char *',

	['@'] = 'id',
	['#'] = 'Class',
	[':'] = 'SEL',

	['['] = array_ctype,    --[Ntype]       ; N = number of elements
	['{'] = struct_ctype,   --{name=type}   ; struct
	['('] = struct_ctype,   --(name=type)   ; union
	['b'] = bitfield_ctype, --bN            ; N = number of bits
	['^'] = pointer_ctype,  --^type         ; pointer
	['?'] = 'void',         --unknown type  ; used for function pointers among other things
}

--convert an objective-c type to a C type.
--deftype specifies how structs should be parsed and returned:
--  'sref' means cdef the structs and then reference them as 'struct <sname>' (for bridgesupport struct definitions)
--  'sref-nodef' is like 'sref' but doesn't cdef the structs (for bridgesupport constants)
--  'tref' means reference structs as '<name>' and don't cdef their definition (when parsing method types)
function type_ctype(s, name, deftype)
	local decoder = ctype_decoders[s:sub(1,1)]
	assert(decoder, s)
	if type(decoder) == 'string' then
		return decoder .. optname(name)
	end
	return decoder(s, name, deftype)
end

local method_ctype = memoize(function(s)
	local ret_ctype
	local arg_ctypes = {}
	for s in s:gmatch'[rnNoORV]?(.-)%d+' do --result-type offset arg1-type offset arg2-type offset ...
		local ctype = type_ctype(s, nil, 'tref')
		if not ret_ctype then
			ret_ctype = ctype
		else
			table.insert(arg_ctypes, ctype)
		end
	end
	local meth_ctype = string.format('%s (*) (%s)', ret_ctype, table.concat(arg_ctypes, ', '))
	if objc.debug then
		log('method_ctype', '%-20s %s', s, meth_ctype)
	end
	return ffi.typeof(meth_ctype)
end)

--bridgesupport file parsing

local xml = {} --{expat_callback = handler}
local tag = {} --{tag = start_tag_handler}

local load_xml --fw. decl

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
	local stype = attrs[typekey] or attrs.type
	if not stype then return end --type not available on this platform
	local ctype = type_ctype(stype, attrs.name, deftype)
	cdef(string.format('%s %s;', typedecl, ctype))
end

function tag.constant(attrs)
	cdef_node(attrs, 'const', 'sref-nodef')
end

function tag.struct(attrs)
	cdef_node(attrs, 'typedef', 'sref')
end

function tag.cftype(attrs)
	cdef_node(attrs, 'typedef', 'sref')
end

function tag.opaque(attrs)
	cdef_node(attrs, 'typedef', 'sref-nodef')
end

function xml.start_tag(name, attrs)
	if tag[name] then
		tag[name](attrs)
	end
end

function load_xml(path)
	expat.parse({path = path}, xml)
end

--loading frameworks

objc.searchpaths = {
	'/System/Library/Frameworks/%s.framework/%s',
	'/Library/Frameworks/%s.framework/%s',
	'~/Library/Frameworks/%s.framework/%s',
}

function objc.load(name, bridgesupport)
	if ffi.os ~= 'OSX' then return end
	for i,fmt in pairs(objc.searchpaths) do
		local path = fmt:format(name, name)
		if canread(path) then
			if bridgesupport ~= false then
				--bridgesupport files contain typedefs and constants which we can't get from the runtime
				local path = fmt:format(name, 'Resources/BridgeSupport/'..name..'.bridgesupport')
				if canread(path) then
					load_xml(path)
				end
				--bridgesupport dylibs contain callable versions of inline functions (NSMakePoint, etc.)
				local path = fmt:format(name, 'Resources/BridgeSupport/'..name..'.dylib')
				if canread(path) then
					ffi.load(path, true)
				end
			end
			ffi.load(path, true)
			if objc.debug then
				log('load', path)
			end
			return
		end
	end
	error(string.format('[objc] %s not found', name))
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
			print(selector_name(sel), types)
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
	if not method and not sel_str:match'[_%:]$' then
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

	local isalloc = sel_str:match'^alloc'
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
		return class(k)
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


if not ... then

objc.debug = 'cdef'
local unit = require'unit'
for i,f in ipairs(dir'2') do
	if f == 'CoreFoundation.bridgesupport' then
		print(('='):rep(80))
		print(f)
		print(('='):rep(80))
		load_xml('2/'..f)
	end
end

--[[
objc.load'Foundation'
objc.load'AppKit'
objc.load'System'
objc.load'CoreServices'

local NSWin = objc.class('NSWin', 'NSWindow')
objc.conforms('NSWin', 'NSWindowDelegate')
]]

--local m = objc.NSWin:alloc()
--print(m)

--:initWithContentRect_styleMask_backing_defer()
--objc.load'Foundation'
--objc.NSHashTable:hashTableWithOptions()
--local a = objc.NSArray:alloc()


end

return objc
