local glue = require'glue'
local objc = require'objc'
local ffi = require'ffi'
local pp = require'pp'

io.stdout:setvbuf'no'
io.stderr:setvbuf'no'

local bsdir = '_bridgesupport' --path where *.bridgesupport files are on Windows (tree or flat doesn't matter)
local luajit = ffi.os == 'Windows' and 'luajit' or './luajit'
local subprocess = true --run each bridgesupport test in a subprocess
local nodeps = true --skip loading dependencies
objc.debug.func_cdef = true
objc.debug.errors = true
objc.debug.ctypes = true
objc.debug.load = true
objc.debug.release = true
objc.debug.redef = true
local default_test = 'bridgesupport'
local default_test_args = {}--{'System'}

local function types_only()
	subprocess = false
	nodeps = true
	objc.debug.func_cdef = false
	objc.debug.errors = true
	objc.debug.methods = true
	objc.debug.load = true
	objc.debug.release = true
	objc.debug.redef = false
end
--types_only()


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
			objc.load_bridgesuport(path, nodeps)
			n = n + 1
			print(n, '', name)
		else
			print('! not found', name)
		end
	end

	--objc.debug = 'cdef'

	if bsfile then
		objc.load(bsfile)
		pp(objc.debug.stats)
	else
		for bsfile in bsfiles() do
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

	objc.load = objc_load --put it back
end

function test.classes()
	if ffi.os ~= 'OSX' then return end

	--objc.debug = 'cdef'
	objc.load'Foundation'
	objc.load'AppKit'
	objc.load'System'
	objc.load'CoreServices'

	local NSWin = objc.class('NSWin', 'NSWindow')
	objc.conforms('NSWin', 'NSWindowDelegate')

	local m = objc.NSWin:alloc():initWithContentRect_styleMask_backing_defer(
		ffi.C.NSMakeRect(100, 100, 500, 300), 0, objc.NSBackingStoreBuffered, false)
	--objc.load'Foundation'
	--objc.NSHashTable:hashTableWithOptions()
	--local a = objc.NSArray:alloc()
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

