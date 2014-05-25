local glue = require'glue'
local objc = require'objc'
local ffi = require'ffi'
local pp = require'pp'

io.stdout:setvbuf'no'
io.stderr:setvbuf'no'

--test options

local subprocess = true --run each bridgesupport test in a subprocess
objc.debug.lazyfuncs = false
objc.debug.checkredef = true
objc.debug.printcdecl = false
objc.debug.loaddeps = true
objc.debug.loadtypes = true
objc.debug.logtopics.method_call = true
objc.debug.logtopics.add_class_method = true

local bsdir = '_bridgesupport' --path where *.bridgesupport files are on Windows (tree or flat doesn't matter)
local luajit = ffi.os == 'Windows' and 'luajit' or './luajit' --luajit command for subprocess running

--test namespace

local test = {} --{name = test_func}

function test.parsing()
	print(objc.type_ctype('[8^c]', 'arr')) --array of pointers
	print(objc.type_ctype('^[8c]', 'arr')) --pointer to array
	print(objc.type_ctype('[8[4c]]', 'arr')) --multi-dim. array
	print(objc.type_ctype('[3^[8^c]]', 'arr'))
	print(objc.type_ctype('{?="x"i"y"i""(?="ux"I"uy"I)}', nil, 'cdef')) --nested unnamed anonymous structs
	print(objc.method_ctype'@"Class"@:{_NSRect={_NSPoint=ff}{_NSSize=ff}}^{?}^?') --unseparated method args
	print''
	print(objc.callback_method_ctype'{_NSPoint=ff}iii') --struct return value not supported
	print(objc.callback_method_ctype'iii{_NSPoint=ff}ii') --pass-by-value struct not supported, stop at first encounter
	print(objc.callback_method_ctype'{_NSPoint=ff}ii{_NSPoint=ff}i') --combined case
end

function test.indent()
	--_NXEvent (test indent for nested unnamed anonymous structs)
	print(objc.type_ctype('{?="type"i"location"{?="x"i"y"i}"time"Q"flags"i"window"I"service_id"Q"ext_pid"i"data"(?="mouse"{?="subx"C"suby"C"eventNum"s"click"i"pressure"C"buttonNumber"C"subType"C"reserved2"C"reserved3"i"tablet"(?="point"{_NXTabletPointData="x"i"y"i"z"i"buttons"S"pressure"S"tilt"{?="x"s"y"s}"rotation"S"tangentialPressure"s"deviceID"S"vendor1"s"vendor2"s"vendor3"s}"proximity"{_NXTabletProximityData="vendorID"S"tabletID"S"pointerID"S"deviceID"S"systemTabletID"S"vendorPointerType"S"pointerSerialNumber"I"uniqueID"Q"capabilityMask"I"pointerType"C"enterProximity"C"reserved1"s})}"mouseMove"{?="dx"i"dy"i"subx"C"suby"C"subType"C"reserved1"C"reserved2"i"tablet"(?="point"{_NXTabletPointData="x"i"y"i"z"i"buttons"S"pressure"S"tilt"{?="x"s"y"s}"rotation"S"tangentialPressure"s"deviceID"S"vendor1"s"vendor2"s"vendor3"s}"proximity"{_NXTabletProximityData="vendorID"S"tabletID"S"pointerID"S"deviceID"S"systemTabletID"S"vendorPointerType"S"pointerSerialNumber"I"uniqueID"Q"capabilityMask"I"pointerType"C"enterProximity"C"reserved1"s})}"key"{?="origCharSet"S"repeat"s"charSet"S"charCode"S"keyCode"S"origCharCode"S"reserved1"i"keyboardType"I"reserved2"i"reserved3"i"reserved4"i"reserved5"[4i]}"tracking"{?="reserved"s"eventNum"s"trackingNum"i"userData"i"reserved1"i"reserved2"i"reserved3"i"reserved4"i"reserved5"i"reserved6"[4i]}"scrollWheel"{?="deltaAxis1"s"deltaAxis2"s"deltaAxis3"s"reserved1"s"fixedDeltaAxis1"i"fixedDeltaAxis2"i"fixedDeltaAxis3"i"pointDeltaAxis1"i"pointDeltaAxis2"i"pointDeltaAxis3"i"reserved8"[4i]}"zoom"{?="deltaAxis1"s"deltaAxis2"s"deltaAxis3"s"reserved1"s"fixedDeltaAxis1"i"fixedDeltaAxis2"i"fixedDeltaAxis3"i"pointDeltaAxis1"i"pointDeltaAxis2"i"pointDeltaAxis3"i"reserved8"[4i]}"compound"{?="reserved"s"subType"s"misc"(?="F"[11f]"L"[11i]"S"[22s]"C"[44c])}"tablet"{?="x"i"y"i"z"i"buttons"S"pressure"S"tilt"{?="x"s"y"s}"rotation"S"tangentialPressure"s"deviceID"S"vendor1"s"vendor2"s"vendor3"s"reserved"[4i]}"proximity"{?="vendorID"S"tabletID"S"pointerID"S"deviceID"S"systemTabletID"S"vendorPointerType"S"pointerSerialNumber"I"uniqueID"Q"capabilityMask"I"pointerType"C"enterProximity"C"reserved1"s"reserved2"[4i]})}', nil, 'cdef'))
end

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

	local objc_load = objc.debug.load_framework --keep it, we'll patch it

	function objc.debug.load_framework(path) --either `name.bridgesupport` or `name.framework` or `name.framework/name`
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
		objc.debug.load_framework(bsfile)
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
					objc.debug.load_framework(bsfile)
				end
				if not subprocess then
					status()
				end
			end
		end
	end

	objc.debug.load_framework = objc_load --put it back
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
	print'ok'
end

function test.errors()
	objc.load'Foundation'
	local function notpcall(...)
		local ok, err = pcall(...)
		assert(not ok)
		print(err)
	end
	notpcall(objc.class, 'NSObject', 'NSString')
	notpcall(objc.class, genname'MyClass', 'MyUnknownClass')
	notpcall(objc.class, genname'MyClass', 'NSObject <MyUnknownProtocol>')
	print'ok'
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
	assert(objc.conforms(cls, 'NSStreamDelegate') == true)
	assert(objc.conforms(cls, 'NSLocking') == true)

	print'ok'
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

	inst, inst2, inst3 = nil
	collectgarbage()

	print'ok'
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

	inst.myinstvar = 'DOH1'
	assert(inst.myinstvar == 'DOH1') --initialized
	inst.myinstvar = 'DOH'
	assert(inst.myinstvar == 'DOH') --updated

	--class vars from instances
	assert(inst.myclassvar == 'doh') --class vars are readable from instances
	inst.myclassvar = 'doh2'
	assert(cls.myclassvar == 'doh2') --and they can be updated from instances
	assert(inst.myclassvar == 'doh2')

	--soft ref counting
	local inst2 = inst:retain()
	assert(inst.myinstvar == 'DOH') --2 refs
	inst = nil
	collectgarbage()
	assert(inst2.myinstvar == 'DOH') --1 ref
	inst2:release() --0 refs; instance gone, vars gone (no way to test, memory was freed)
	assert(cls.myclassvar == 'doh2') --class vars still there

	print'ok'
end

function test.override()
	objc.load'Foundation'

	local classname = genname'MyClass'
	local cls = objc.class(classname, 'NSObject')
	local obj = cls:new()
	local instdesc = 'hello-instance'
	local classdesc = 'hello-class'

	function cls:description() --override the instance method
		return objc.NSString:alloc():initWithUTF8String(instdesc)
	end
	assert(ffi.string(cls:description():UTF8String()) == classname) --class method was not overriden
	assert(ffi.string(obj:description():UTF8String()) == instdesc) --instance method was overriden

	--objc.debug.invalidate_class(cls)

	local metacls = objc.metaclass(cls)
	function metacls:description() --override the class method
		return objc.NSString:alloc():initWithUTF8String(classdesc)
	end
	assert(ffi.string(cls:description():UTF8String()) == classdesc) --class method was overriden
	assert(ffi.string(obj:description():UTF8String()) == instdesc) --instance method was not overriden again

	print'ok'
end

function test.ivars()
	objc.load'Foundation'

	local function print_ivars(cls)
		for ivar in objc.ivars(cls) do
			print(cls, ivar:name(), tonumber(ivar:offset()), ivar:type(), ivar:ctype())
		end
	end

	local obj = objc.NSDocInfo:new()

	print_ivars(obj:class())

	if ffi.abi'64bit' then
		assert(ffi.typeof(obj.time) == ffi.typeof'long long')
	else
		assert(type(obj.time) == 'number')
	end
	assert(type(obj.mode) == 'number') --unsigned short
	assert(ffi.typeof(obj.flags) == ffi.typeof(obj.flags)) --anonymous struct (assert that it was cached)

	obj.time = 123
	assert(obj.time == 123)

	assert(obj.flags.isDir == 0)
	obj.flags.isDir = 3 --1 bit
	assert(obj.flags.isDir == 1) --1 bit was set (so this is not a luavar or anything)

	print'ok'
end

function test.properties()
	objc.load'Foundation'

	local function print_properties(cls)
		for prop in objc.properties(cls) do
			print(string.format('%-30s %-36s %-20s %-40s %-40s',
					cls, prop:name(), prop:ctype(), prop:getter(), prop:setter() or 'n/a'))
		end
	end

	print_properties(objc.NSProgress)

	local pr = objc.NSProgress:progressWithTotalUnitCount(123)
	assert(pr.totalUnitCount == 123) --as initialized
	pr.totalUnitCount = 321 --read/write property
	assert(pr.totalUnitCount == 321)
	assert(not pcall(function() pr.indeterminate = 1 end)) --attempt to set read/only property
	assert(pr.indeterminate == 0)

	print'ok'
end

function test.blocks()
	objc.load'Foundation'

	local s = objc.NSString:alloc():initWithUTF8String('line1\nline2\nline3')
	local t = {}
	local block, callback = objc.block(function(line, pstop)
		t[#t+1] = ffi.string(line:UTF8String())
		if #t == 2 then --stop at line 2
			pstop[0] = 1
		end
	end, 'v@^B')
	s:enumerateLinesUsingBlock(block)
	callback:free()
	assert(#t == 2)
	assert(t[1] == 'line1')
	assert(t[2] == 'line2')

	print'ok'
end

function test.methods()
	objc.load'Foundation'

	local function print_methods(cls, name)
		for meth in objc.methods(cls) do
			print(string.format('%-50s %-50s %s', meth:name(), meth:type(), meth:ctype_string()))
		end
	end

	print_methods(objc.NSString)

	local m = objc.method(objc.NSString, objc.SEL'initWithUTF8String:')
	print(m:type(), m:ctype_string())

	print'ok'
end

function test.protocols()
	objc.load'Foundation'
	objc.load'AppKit'

	for name, mtype in objc.protocol('NSObject'):methods(true, true) do
		print(string.format('%-50s %s', name, mtype))
	end
end

function test.tolua()
	objc.load'Foundation'

	local n = objc.toobj(123.5)
	assert(objc.issubclass(n.isa, objc.NSNumber))
	assert(objc.tolua(n) == 123.5)

	local s = objc.toobj'hello'
	assert(objc.issubclass(s.isa, objc.NSString))
	assert(objc.tolua(s) == 'hello')

	local a = {1,2,6,7}
	local t = objc.toobj(a)
	assert(t:count() == #a)
	for i=1,#a do
		assert(t:objectAtIndex(i-1):doubleValue() == a[i])
	end
	a = objc.tolua(t)
	assert(#a == 4)
	assert(a[3] == 6)

	local d = {a = 1, b = 'baz', d = {1,2,3}, [{x=1}] = {y=2}}
	local t = objc.toobj(d)
	assert(t:count() == 4)
	assert(objc.tolua(t:valueForKey(objc.toobj'a')) == d.a)
	assert(objc.tolua(t:valueForKey(objc.toobj'b')) == d.b)
	assert(objc.tolua(t:valueForKey(objc.toobj'd'))[2] == 2)
	print(ffi.string(t:description():UTF8String()))

	print'ok'
end

function test.windows()
	objc.load'Foundation'
	objc.load'AppKit'

	local pool = objc.NSAutoreleasePool:new()
	local NSApp = objc.class('NSApp', 'NSApplication <NSApplicationDelegate>')

	--we need to add methods to the class before creating any objects!
	--note: NSApplicationDelegate is an informal protocol.

	function NSApp:applicationShouldTerminateAfterLastWindowClosed()
		print'last window closed...'
		return true
	end

	function NSApp:applicationShouldTerminate()
		print'terminating...'
		return true
	end

	local app = NSApp:sharedApplication()
	app:setDelegate(app)
	app:setActivationPolicy(objc.NSApplicationActivationPolicyRegular)

	local NSWin = objc.class('NSWin', 'NSWindow <NSWindowDelegate>')

	--we need to add methods to the class before creating any objects!
	--note: NSWindowDelegate is a formal protocol.

	function NSWin:windowWillClose()
		print'window will close...'
	end

	local style = bit.bor(
						objc.NSTitledWindowMask,
						objc.NSClosableWindowMask,
						objc.NSMiniaturizableWindowMask,
						objc.NSResizableWindowMask)

	local win = objc.NSWin:alloc():initWithContentRect_styleMask_backing_defer(
						objc.NSMakeRect(100, 100, 500, 300), style, objc.NSBackingStoreBuffered, false)
	win:setDelegate(win)

	app:activateIgnoringOtherApps(true)
	win:makeKeyAndOrderFront(nil)
	app:run()
end

function test.all(...)
	for k,v in glue.sortedpairs(test) do
		if k ~= 'all' and k ~= 'bridgesupport' then
			print(k)
			print(('='):rep(80))
			test[k](...)
		end
	end
end

--cmdline interface

local test_name = ...

if not test_name then
	print('Usage: '..luajit..' '..arg[0]..' <test>')
	print'Available tests:'
	for k in glue.sortedpairs(test) do
		print('', k)
	end
else
	test[test_name](select(2, ...))
end

