local glue = require'glue'
local objc = require'objc'
local ffi = require'ffi'

io.stdout:setvbuf'no'
io.stderr:setvbuf'no'

objc.debug = true

local bsdir = '_bridgesupport' --path where *.bridgesupport files are on Windows

--test parsing of bridgesupport files.
--works on Windows too - just copy your bridgesupport files into `bsdir`.
local function test_bridgesupport()

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
			objc.load_bridgesuport(path)
			n = n + 1
			print(n, '', name)
		else
			print('! not found', name)
		end
	end

	--objc.debug = 'cdef'

	for bsfile in bsfiles() do
		objc.load(bsfile)
	end

	objc.load = objc_load --put it back
end

local function test_classes()
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

--test_bridgesupport()
test_classes()

