--objecive-c runtime binding (Cosmin Apreutesei, public domain).
--ideas and code from TLC by Fjölnir Ásgeirsson (c) 2012, MIT license.
--tested with with LuaJIT 2.0.3, 32bit and 64bit on OSX 10.9.

local ffi = require'ffi'
local bit = require'bit'
local expat = require'expat'

assert(ffi.os == 'OSX', 'platform not OSX')

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

--helpers

local function ptr(p)
	if p == nil then return nil end
	return p
end

local function nptr(p)
	if p == nil then return nil end
	return tonumber(ffi.cast('intptr_t', p))
end

local function memoize(func)
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

local function log(topic, fmt, ...)
	print(string.format('[objc] %-20s %s', topic, string.format(fmt, ...)))
end

local C = ffi.C
local objc = {C = C}

--debugging

objc.debug = false

--loading frameworks

objc.searchpaths = {
	'/System/Library/Frameworks/%s.framework/%s',
	'/Library/Frameworks/%s.framework/%s',
	'~/Library/Frameworks/%s.framework/%s',
}

function objc.load(name)
	local canread = bit.lshift(1,2)
	for i,fmt in pairs(objc.searchpaths) do
		local path = string.format(fmt, name, name)
		if C.access(path, canread) == 0 then
			return ffi.load(path, true)
		end
	end
	error(string.format('[objc] %s not found', name))
end

if ffi.arch ~= 'arm' then
	ffi.load('libobjc.A.dylib', true)
	objc.load'CoreFoundation'
	objc.load'Foundation'
	objc.load'Carbon'
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

--type parsing

local type_ctype --fw. decl.

local function array_ctype(s) --[ntype] (number of elements followed by element type)
	local n,s = s:match'.^(%d+)(.-)%]$'
	return string.format('%s[%d]', type_ctype(s), n)
end

local function union_struct_ctype(s, kw)
	local name = s:match'^.([^%=]+)'
	return string.format('%s %s', kw, name)
end

local function union_ctype(s) --(name=type)
	return union_struct_ctype(s, 'union')
end

local function struct_ctype(s) --{name=type}
	return union_struct_ctype(s, 'struct')
end

local function bitfield_ctype(s)
	error'NYI'
end

local function pointer_ctype(s)
	return type_ctype(s:sub(2)) .. ' *'
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

	['['] = array_ctype,    --[ntype] (number of elements followed by element type)
	['{'] = struct_ctype,   --{name=type}
	['('] = union_ctype,    --(name=type)
	['b'] = bitfield_ctype, --bnum (number of bits)
	['^'] = pointer_ctype,  --^type
	['?'] = 'void *',       --unknow type (function pointer, etc.)
}

function type_ctype(s)
	local decoder = ctype_decoders[s:sub(1, 1)]
	assert(decoder, s)
	return type(decoder) == 'string' and decoder or decoder(s)
end

local method_ctype = memoize(function(s)

	local ret_ctype
	local arg_ctypes = {}
	for s in s:gmatch'[rnNoORV]?(.-)%d+' do --result-type offset arg1-type offset arg2-type offset ...
		local ctype = type_ctype(s)
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

--BridgeSupport

--[[
local typekey = ffi.abi'64bit' and 'type64' or 'type'

local _curObj = nil
local _parseCallbacks = {
	start_tag = function(name, attrs)
		-- Methods and classes don't really need to be loaded (keeping these here in case I find otherwise)
		--elseif name == "class" then
		--elseif name == "method" then
		--elseif name == "arg" then
		--elseif name == "retval" then
		--elseif name == "signatures" then
		if name == "string_constant" then
			rawset(bs, attrs.name, attrs.value)
			if _loadingGlobally == true then _G[attrs.name] = rawget(bs, attrs.name) end
		elseif name == "enum" then
			rawset(bs, attrs.name, tonumber(attrs.value))
			if _loadingGlobally == true then _G[attrs.name] = rawget(bs, attrs.name) end
		elseif name == "struct" then
			if name_clashes[attrs.name] then return end
			local type = objc.parseTypeEncoding(attrs[typeKey] or attrs.type)
			if type ~= nil and #type > 0 then
				type = objc.typeToCType(type[1], nil, true)
				if type ~= nil then
					--print("typedef "..type.." "..attrs.name)
					local success, err = pcall(ffi.cdef, "typedef "..type.." "..attrs.name)
					if success == false then
						print("[bs] Error loading struct "..attrs.name..": "..err)
					else
						rawset(bs, attrs.name, ffi.typeof(attrs.name))
						if _loadingGlobally == true then _G[attrs.name] = rawget(bs, attrs.name) end
					end
				end
			end
		--elseif name == "field" then
		elseif name == "cftype" then
			local type = objc.parseTypeEncoding(attrs[typeKey] or attrs.type)
			if type ~= nil and #type > 0 then
				type = objc.typeToCType(type[1], attrs.name)
				if type ~= nil then
					--_log("typedef "..type)
					ffi.cdef("typedef "..type)
				end
			end
		elseif name == "constant" then
			local type = objc.parseTypeEncoding(attrs[typeKey] or attrs.type)
			if type ~= nil and #type > 0 then
				type = objc.typeToCType(type[1], attrs.name)
				if type ~= nil then
					--_log(type)
					ffi.cdef(type)
				end
			end
		elseif name == "function" then
			_curObj = {}
			_curObj.type = "function"
			_curObj.name = attrs.name
			_curObj.args = {}
			_curObj.retval = "v"
		elseif _curObj ~= nil and name == "arg" then
			local type = attrs[typeKey]  or attrs.type
			if type == "@?" then type = "@" end -- Apple seems to have gone crazy and added a weird special case for block definitions
			table.insert(_curObj.args, type)
		elseif _curObj ~= nil and name == "retval" then
			_curObj.retval = attrs[typeKey] or attrs.type
		elseif name == "depends_on" then
			-- If dependency loading is off we still load nested frameworks
			local n = 0
			for o in attrs.path:gfind(".framework") do n=n+1 end
			if bs.loadDependencies == true or n >= 2 then
				bs.loadFramework(attrs.path, _loadingGlobally, true)
			end
		--elseif name == "opaque" then
		--elseif name == "informal_protocol" then
		--elseif name == "function_alias" then
		end
	end,
	end_tag = function(name)
		if name == "function" then
			local sig       = _curObj.retval..table.concat(_curObj.args)
			local signature = objc.impSignatureForTypeEncoding(sig, _curObj.name)
			local name = _curObj.name
			--print(signature)
			if signature ~= nil then
					bs[name] = function(...)
						bs[name] = ffi.cdef(signature)
					if _loadingGlobally == true then
						_G[name] = C[name]
					end
					return bs[name](...)
				end
				if _loadingGlobally == true then
					_G[name] = bs[name]
				end
			end
			_curObj = nil
		end
	end
}
]]

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
			error(string.format('%s\n%s', ret, debug.traceback()))
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


if not ... then

objc.debug = true

objc.load'Foundation'
objc.load'AppKit'
objc.load'System'
objc.load'CoreServices'

local NSWin = objc.class('NSWin', 'NSWindow')
objc.conforms('NSWin', 'NSWindowDelegate')

--local m = objc.NSWin:alloc()
--print(m)

--:initWithContentRect_styleMask_backing_defer()
--objc.load'Foundation'
--objc.NSHashTable:hashTableWithOptions()
--local a = objc.NSArray:alloc()


end

return objc
