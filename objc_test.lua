local glue = require'glue'
local objc = require'objc'
local ffi = require'ffi'
local pp = require'pp'

io.stdout:setvbuf'no'
io.stderr:setvbuf'no'

local bsdir = '_bridgesupport' --path where *.bridgesupport files are on Windows (tree or flat doesn't matter)
local luajit = ffi.os == 'Windows' and 'luajit' or './luajit'
local subprocess = false --run each bridgesupport test in a subprocess
objc.debug.types = true
objc.debug.deps = false --skip loading dependencies
objc.debug.func_cdef = false --cdef functions immediately instead of lazily
objc.debug.errors = true
objc.debug.ctypes = true
objc.debug.load = true --no effect: we redefine objc.load
objc.debug.release = true
objc.debug.redef = true --report redefinitions (slower)
objc.debug.methods = false
local default_test = 'classes'
local default_test_args = {}--{'/System/Library/Frameworks/OpenGL.framework'}

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
					ffi.load(dpath, true)
				end

				--load the dylib with inlines first (needed for function aliases)
				local dpath = path:gsub('bridgesupport$', 'dylib')
				print(path, dpath)
				if glue.fileexists(dpath) then
					ffi.load(dpath, true)
				end
			end

			objc.load_bridgesuport(path)
			n = n + 1
			print(n, '', name)
		else
			print('! not found', name, path)
		end
	end

	if bsfile then
		objc.load(bsfile)
		pp(objc.debug.stats)
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
					pp(objc.debug.stats)
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
	assert(tostring(objc.SEL'se_lec_tor') == 'se:lec:tor')
	assert(tostring(objc.SEL'se_lec_tor_') == 'se:lec:tor:')
	assert(tostring(objc.SEL'__se_lec_tor') == '__se:lec:tor')
	assert(tostring(objc.SEL'__se:lec:tor:') == '__se:lec:tor:')
end

function test.errors()
	objc.load'Foundation'
	print(pcall(objc.class, 'NSObject'))
	print(pcall(objc.class, genname'MyClass', 'MyUnknownClass'))
	print(pcall(objc.class, genname'MyClass', 'NSObject <MyUnknownProtocol>'))

end

function test.newclass()
	local cls = objc.class'MyClassX'
	assert(tostring(cls) == 'MyClassX')

	local cls = objc.class(genname'MyClass', 'NSArray')


	local cls = objc.class(genname'MyClass', 'NSArray <NSStreamDelegate>')


end

function test.inspect()
	objc.inspect_protocol'NSFilePresenter'
	local cls = objc.class(genname'MyClass', 'NSArray <NSStreamDelegate>')
	objc.inspect_class(cls)
end

function test.inspect_nswindow()
	objc.load'Foundation'
	objc.load'AppKit'
	objc.load'System'
	objc.load'CoreServices'
	objc.inspect_class'NSWindow'
end


function test.properties()
	objc.load'Foundation'
	objc.load'AppKit'
	objc.load'System'
	objc.load'CoreServices'

	local NSApp = objc.class('NSApp', 'NSApplication')
	local nsapp = NSApp:sharedApplication()
	nsapp.delegate = nsapp
end


function test.classes()

	--[[
	test.selectors()
	test.errors()
	test.newclass()
	test.inspect()
	]]
	test.inspect_nswindow()

	--local objc.NSArray

	--[[
	objc.load('Foundation', 'nodeps')
	objc.load('AppKit', 'nodeps')
	objc.load('System', 'nodeps')
	objc.load('CoreServices', 'nodeps')

	--local pool = objc.NSAutoreleasePool:new()
	local NSApp = objc.class('NSApp', 'NSApplication')
	local nsapp = NSApp:sharedApplication()
	nsapp.delegate = nsapp

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
	]]
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

