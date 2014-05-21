local glue = require'glue'
local objc = require'objc'
local ffi = require'ffi'
local pp = require'pp'

io.stdout:setvbuf'no'
io.stderr:setvbuf'no'

--test options

local default_test = 'bridgesupport'
local default_test_args = {}--{'/System/Library/Frameworks/OpenGL.framework'}

local subprocess = false --run each bridgesupport test in a subprocess
objc.debug.lazyfuncs = false
objc.debug.checkredef = true
objc.debug.printdecl = false
objc.debug.loaddeps = false
objc.debug.loadtypes = true

local bsdir = '_bridgesupport' --path where *.bridgesupport files are on Windows (tree or flat doesn't matter)
local luajit = ffi.os == 'Windows' and 'luajit' or './luajit' --luajit command for subprocess running

--test namespace

local test = {} --{name = test_func}

--test parsing of bridgesupport files.
--works on Windows too - just copy your bridgesupport files into whatever you set `bsdir` above.
function test.bridgesupport(bsfile)

	local function list_func(cmd)
		return function()
			return coroutine.wrap(function()
				local f = io.popen(cmd)
				for s in f:lines() do
					coroutine.yield(s)
				end
				f:close()
			end)
		end
	end

	local bsfiles
	if ffi.os == 'Windows' then
		bsfiles = list_func('dir /B /S '..bsdir..'\\*.bridgesupport')
	elseif ffi.os == 'OSX' then
		bsfiles = list_func('find /System/Library/frameworks -name \'*.bridgesupport\'')
	else
		error'can\'t run on this OS'
	end

	local loaded = {}
	local n = 0

	local objc_load = objc.load --keep it, we'll patch it

	function objc.load(path) --either `name.bridgesupport` or `name.framework` or `name.framework/name`
		local name
		if path:match'%.bridgesupport$' then
			name = path:match'([^/\\]+)%.bridgesupport$'
		else
			name = path:match'/([^/]+)%.framework$' or path:match'([^/]+)$'
			if ffi.os == 'Windows' then
				path = bsdir..'\\'..name..'.bridgesupport'
			else
				path = path .. '/Resources/BridgeSupport/' .. name .. '.bridgesupport'
			end
		end
		if loaded[name] then return end
		loaded[name] = true
		if glue.fileexists(path) then

			if ffi.os == 'OSX' then
				--load the dylib first (needed for function aliases)
				local dpath = path:gsub('Resources/BridgeSupport/.*$', name)
				if glue.fileexists(dpath) then
					pcall(ffi.load, dpath, true)
				end

				--load the dylib with inlines first (needed for function aliases)
				local dpath = path:gsub('bridgesupport$', 'dylib')
				print(path, dpath)
				if glue.fileexists(dpath) then
					pcall(ffi.load, dpath, true)
				end
			end

			objc.debug.load_bridgesupport(path)
			n = n + 1
			print(n, '', name)
		else
			print('! not found', name, path)
		end
	end

	local function status()
		pp('errors', objc.debug.errcount)
		print('globals: '..objc.debug.cnames.global[1])
		print('structs: '..objc.debug.cnames.struct[1])
	end

	if bsfile then
		objc.load(bsfile)
		status()
	else
		for bsfile in bsfiles() do
			if bsfile:match'Python' then
				print('skipping '..bsfile) --python bridgesupport files are non-standard and deprecated
			else
				print(); print(bsfile); print(('='):rep(80))
				if subprocess then
					os.execute(luajit..' '..arg[0]..' bridgesupport '..bsfile)
				else
					objc.load(bsfile)
				end
				if not subprocess then
					status()
				end
			end
		end
	end

	objc.load = objc_load --put it back
end

local n = 0
local function genname(prefix)
	n = n + 1
	return prefix..n
end

function test.selectors()
	assert(tostring(objc.selector'se_lec_tor') == 'se:lec:tor')
	assert(tostring(objc.selector'se_lec_tor_') == 'se:lec:tor:')
	assert(tostring(objc.selector'__se_lec_tor') == '__se:lec:tor')
	assert(tostring(objc.selector'__se:lec:tor:') == '__se:lec:tor:')
end

function test.errors()
	objc.load'Foundation'
	print(pcall(objc.class, 'NSObject'))
	print(pcall(objc.class, genname'MyClass', 'MyUnknownClass'))
	print(pcall(objc.class, genname'MyClass', 'NSObject <MyUnknownProtocol>'))
end

function test.newclass()
	objc.load'Foundation'
	local cls = objc.class('MyClassX', false) --root class
	assert(tostring(cls) == 'MyClassX')
	assert(not objc.superclass(cls))
	local cls = objc.class(genname'MyClass', 'NSArray') --derived class
	assert(tostring(objc.superclass(cls)) == 'NSArray')
	local cls = objc.class(genname'MyClass', 'NSArray <NSStreamDelegate, NSLocking>') --derived + conforming
	assert(tostring(objc.superclass(cls)) == 'NSArray')
	assert(objc.class_conforms(cls, objc.protocol'NSStreamDelegate') == true)
	assert(objc.class_conforms(cls, objc.protocol'NSLocking') == true)
end

function test.refcount()
	objc.load'Foundation'
	local cls = objc.class(genname'MyClass', 'NSObject')
	local inst, inst2, inst3

	inst = cls:new()
	assert(inst:retainCount() == 1)

	inst2 = inst:retain() --same class, new cdata, new reference
	assert(inst:retainCount() == 2)

	inst3 = inst:retain()
	assert(inst:retainCount() == 3)

	inst3 = nil --release() on gc
	collectgarbage()
	assert(inst:retainCount() == 2)

	inst3 = inst:retain()
	assert(inst:retainCount() == 3)

	inst:release() --manual release()
	assert(inst:retainCount() == 2)

	inst = nil --object already disowned by inst, refcount should not decrease
	collectgarbage()
	assert(inst2:retainCount() == 2)
end

function test.luavars()
	objc.load'Foundation'
	local cls = objc.class(genname'MyClass', 'NSObject')

	--class vars
	cls.myclassvar = 'doh1'
	assert(cls.myclassvar == 'doh1') --intialized
	cls.myclassvar = 'doh'
	assert(cls.myclassvar == 'doh') --updated

	--inst vars
	local inst = cls:new()

	inst.myinstvar = "do'h1"
	assert(inst.myinstvar == "do'h1") --initialized
	inst.myinstvar = "do'h"
	assert(inst.myinstvar == "do'h") --updated

	--class vars from instances
	assert(inst.myclassvar == 'doh') --class vars are readable from instances
	inst.myclassvar = 'doh2'
	assert(cls.myclassvar == 'doh') --but they can't be updated from instances
	assert(inst.myclassvar == 'doh2') --all we did was to shadow the class var with an instance var
end

function test.override()
	objc.load'Foundation'
	local classname = genname'MyClass'
	local cls = objc.class(classname, 'NSObject')

	--objc.debug.logtopics.method_ctype = true

	local s = 'hello-instance'
	function cls:description() --override the instance method
		return objc.NSString:alloc():initWithUTF8String(ffi.cast('char*', s))
	end
	assert(ffi.string(cls:description():UTF8String()) == classname) --use original class method
	local obj = cls:new()
	assert(ffi.string(obj:description():UTF8String()) == s) --use overriden

	--objc.debug.invalidate_class(cls)

	local s = 'hello-class'
	local metacls = objc.metaclass(cls)
	--print(ffi.cast('uintptr_t', metacls), ffi.cast('uintptr_t', cls))
	function metacls:description() --override the class method
		return objc.NSString:alloc():initWithUTF8String(ffi.cast('char*', s))
	end
	assert(ffi.string(cls:description():UTF8String()) == s) --use overriden class method

end




function test.properties()
	objc.load'Foundation'
	objc.load'AppKit'
	objc.load'System'
	objc.load'CoreServices'

	local NSApp = objc.class('NSApp', 'NSApplication')
	local nsapp = NSApp:sharedApplication()
	objc.inspect.class'NSApplication'
	nsapp:setDelegate(nsapp)
end

function test.methods()
	objc.load'Foundation'
	objc.load'AppKit'
	objc.load'System'
	objc.load'CoreServices'

	local NSApp = objc.class('NSApp', 'NSApplication')
	local nsapp = NSApp:sharedApplication()
	objc.inspect.class'NSApplication'
	nsapp:setDelegate(nsapp)
end

function test.classes()
	objc.load'Foundation'
	objc.load'AppKit'
	objc.load'System'
	objc.load'CoreServices'

	--local pool = objc.NSAutoreleasePool:new()
	local NSApp = objc.class('NSApp', 'NSApplication')
	local nsapp = NSApp:sharedApplication()
	nsapp:setDelegate(nsapp)

	local NSWin = objc.class('NSWin', 'NSWindow')
	objc.conforms('NSWin', 'NSWindowDelegate')

	local style = bit.bor(
						objc.NSTitledWindowMask,
						objc.NSClosableWindowMask,
						objc.NSMiniaturizableWindowMask,
						objc.NSResizableWindowMask)

	local win = objc.NSWin:alloc():initWithContentRect_styleMask_backing_defer(
						objc.NSMakeRect(100, 100, 500, 300), style, objc.NSBackingStoreBuffered, false)

	nsapp:activateIgnoringOtherApps(true)
	win:makeKeyAndOrderFront(nil)
	nsapp:run()
end

function test.inspect(cmd, ...)
	objc.load'Foundation'
	objc.load'AppKit'
	objc.load'System'
	objc.load'CoreServices'
	objc.inspect[cmd](...)
end

function test.inspect_class()
	objc.load'Foundation'
	objc.load'AppKit'
	objc.load'System'
	objc.load'CoreServices'
	objc.inspect.class'NSWindow'
end

function test.inspect_protocol()
	objc.load'Foundation'
	objc.inspect.protocol'NSFilePresenter'
end

--cmdline interface

local test_name = ...
local test_args = {select(2, ...)}
if test_name == 'objc_test' then test_name = nil end --loaded as module
if test_name == nil then
	test_name = default_test
	test_args = default_test_args
end

if not test_name then
	print('Usage: '..luajit..' '..arg[0]..' <test>')
	print'Available tests:'
	for k in glue.sortedpairs(test) do
		print('', k)
	end
else
	test[test_name](unpack(test_args))
end

