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

local bsdir = '_bridgesupport' --path where *.bridgesupport files are on Windows (tree or flat doesn't matter)
local luajit = ffi.os == 'Windows' and 'luajit' or './luajit' --luajit command for subprocess running

if ffi.os == 'OSX' then
	objc.load'Foundation'
end

--test helpers

local function printf(...)
	print(string.format(...))
end

local function hr()
	print(('-'):rep(80))
end

local n = 0
local function genname(prefix)
	n = n + 1
	return prefix..n
end

local function classname()
	return genname'MyClass'
end

local function errpcall(patt, ...) --pcall that should fail
	local ok, err = pcall(...)
	assert(not ok)
	assert(err:find(patt))
end

--test namespace

local test = {}      --{name = test_func}
local eyetest = {} --{name = test_func}

function test.parsing()
	assert(objc.type_ctype('[8^c]', 'arr') == 'char *arr[8]') --array of pointers
	assert(objc.type_ctype('^[8c]', 'arr') == 'char (*arr)[8]') --pointer to array
	assert(objc.type_ctype('[8[4c]]', 'arr') == 'char arr[8][4]') --multi-dim. array
	assert(objc.type_ctype('[3^[8^c]]', 'arr') == 'char *(*arr[3])[8]')
	assert(objc.type_ctype('{?="x"i"y"i""(?="ux"I"uy"I)}', nil, 'cdef') ==
		'struct {\n\tint x;\n\tint y;\n\tunion {\n\t\tunsigned int ux;\n\t\tunsigned int uy;\n\t};\n}'
		) --nested unnamed anonymous structs
	assert(objc.method_ctype'@"Class"@:{_NSRect={_NSPoint=ff}{_NSSize=ff}}^{?}^?',
		'id (*) (id, SEL, struct _NSRect, void *, void *)') --unseparated method args
	assert(objc.method_ctype('{_NSPoint=ff}iii', true) ==
		'void (*) (int, int, int)') --struct return value not supported
	assert(objc.method_ctype('iii{_NSPoint=ff}ii', true) ==
		'int (*) (int, int)') --pass-by-value struct not supported, stop at first encounter
	assert(objc.method_ctype('{_NSPoint=ff}ii{_NSPoint=ff}i', true) ==
		'void (*) (int, int)') --combined case
end

function eyetest.indent()
	--_NXEvent (test indent for nested unnamed anonymous structs)
	print(objc.type_ctype('{?="type"i"location"{?="x"i"y"i}"time"Q"flags"i"window"I"service_id"Q"ext_pid"i"data"(?="mouse"{?="subx"C"suby"C"eventNum"s"click"i"pressure"C"buttonNumber"C"subType"C"reserved2"C"reserved3"i"tablet"(?="point"{_NXTabletPointData="x"i"y"i"z"i"buttons"S"pressure"S"tilt"{?="x"s"y"s}"rotation"S"tangentialPressure"s"deviceID"S"vendor1"s"vendor2"s"vendor3"s}"proximity"{_NXTabletProximityData="vendorID"S"tabletID"S"pointerID"S"deviceID"S"systemTabletID"S"vendorPointerType"S"pointerSerialNumber"I"uniqueID"Q"capabilityMask"I"pointerType"C"enterProximity"C"reserved1"s})}"mouseMove"{?="dx"i"dy"i"subx"C"suby"C"subType"C"reserved1"C"reserved2"i"tablet"(?="point"{_NXTabletPointData="x"i"y"i"z"i"buttons"S"pressure"S"tilt"{?="x"s"y"s}"rotation"S"tangentialPressure"s"deviceID"S"vendor1"s"vendor2"s"vendor3"s}"proximity"{_NXTabletProximityData="vendorID"S"tabletID"S"pointerID"S"deviceID"S"systemTabletID"S"vendorPointerType"S"pointerSerialNumber"I"uniqueID"Q"capabilityMask"I"pointerType"C"enterProximity"C"reserved1"s})}"key"{?="origCharSet"S"repeat"s"charSet"S"charCode"S"keyCode"S"origCharCode"S"reserved1"i"keyboardType"I"reserved2"i"reserved3"i"reserved4"i"reserved5"[4i]}"tracking"{?="reserved"s"eventNum"s"trackingNum"i"userData"i"reserved1"i"reserved2"i"reserved3"i"reserved4"i"reserved5"i"reserved6"[4i]}"scrollWheel"{?="deltaAxis1"s"deltaAxis2"s"deltaAxis3"s"reserved1"s"fixedDeltaAxis1"i"fixedDeltaAxis2"i"fixedDeltaAxis3"i"pointDeltaAxis1"i"pointDeltaAxis2"i"pointDeltaAxis3"i"reserved8"[4i]}"zoom"{?="deltaAxis1"s"deltaAxis2"s"deltaAxis3"s"reserved1"s"fixedDeltaAxis1"i"fixedDeltaAxis2"i"fixedDeltaAxis3"i"pointDeltaAxis1"i"pointDeltaAxis2"i"pointDeltaAxis3"i"reserved8"[4i]}"compound"{?="reserved"s"subType"s"misc"(?="F"[11f]"L"[11i]"S"[22s]"C"[44c])}"tablet"{?="x"i"y"i"z"i"buttons"S"pressure"S"tilt"{?="x"s"y"s}"rotation"S"tangentialPressure"s"deviceID"S"vendor1"s"vendor2"s"vendor3"s"reserved"[4i]}"proximity"{?="vendorID"S"tabletID"S"pointerID"S"deviceID"S"systemTabletID"S"vendorPointerType"S"pointerSerialNumber"I"uniqueID"Q"capabilityMask"I"pointerType"C"enterProximity"C"reserved1"s"reserved2"[4i]})}', nil, 'cdef'))
end

--test parsing of bridgesupport files.
--works on Windows too - just copy your bridgesupport files into whatever you set `bsdir` above.
function eyetest.bridgesupport(bsfile)

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

function test.selectors()
	assert(tostring(objc.SEL'se_lec_tor') == 'se:lec:tor')
	assert(tostring(objc.SEL'se_lec_tor_') == 'se:lec:tor:')
	assert(tostring(objc.SEL'__se_lec_tor') == '__se:lec:tor')
	assert(tostring(objc.SEL'__se:lec:tor:') == '__se:lec:tor:')
end

--class, superclass, metaclass, class protocols
function test.class()
	--arg. checking
	errpcall('already',    objc.class, 'NSObject', 'NSString')
	errpcall('superclass', objc.class, classname(), 'MyUnknownClass')
	errpcall('protocol',   objc.class, classname(), 'NSObject <MyUnknownProtocol>')

	--class overloaded constructors
	local cls = objc.class('MyClassX', false) --root class
	assert(objc.classname(cls) == 'MyClassX')
	assert(not objc.superclass(cls))

	--derived class
	local cls = objc.class(classname(), 'NSArray')
	assert(objc.isa(cls, 'NSArray'))

	--derived + conforming
	local cls = objc.class(classname(), 'NSArray <NSStreamDelegate, NSLocking>')
	assert(objc.isa(cls, 'NSArray'))

	assert(objc.conforms(cls, 'NSStreamDelegate'))
	assert(objc.conforms(cls, 'NSLocking'))

	local t = {0}
	for proto in objc.protocols(cls) do
		t[proto:name()] = true
		t[1] = t[1] + 1
	end
	assert(t[1] == 2)
	assert(t.NSStreamDelegate)
	assert(t.NSLocking)

	--class hierarchy queries
	assert(objc.superclass(cls) == objc.NSArray)
	assert(objc.metaclass(cls))
	assert(objc.superclass(objc.metaclass(cls)) == objc.metaclass'NSArray')
	assert(objc.metaclass(objc.superclass(cls)) == objc.metaclass'NSArray')
	assert(objc.metaclass(objc.metaclass(cls)) == nil)
	assert(objc.isa(cls, 'NSObject'))
	assert(objc.ismetaclass(objc.metaclass(cls)))
	assert(objc.isclass(cls))
	assert(not objc.ismetaclass(cls))
	assert(not objc.isobj(cls))
	assert(objc.isclass(objc.metaclass(cls)))

	local obj = cls:new()
	assert(objc.isobj(obj))
	assert(not objc.isclass(obj))
end

function test.refcount()
	local cls = objc.class(classname(), 'NSObject')
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
end

function test.luavars()
	local cls = objc.class(classname(), 'NSObject')

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
end

function test.override()
	local cls = objc.class(classname(), 'NSObject')
	local metacls = objc.metaclass(cls)
	local obj = cls:new()
	local instdesc = 'hello-instance'
	local classdesc = 'hello-class'

	function metacls:description() --override the class method
		return objc.NSString:alloc():initWithUTF8String(classdesc)
	end

	function cls:description() --override the instance method
		return objc.NSString:alloc():initWithUTF8String(instdesc)
	end

	assert(cls:description():UTF8String() == classdesc) --class method was overriden
	assert(obj:description():UTF8String() == instdesc) --instance method was overriden and it's different

	--subclass and test again

	local cls2 = objc.class(classname(), cls)
	local metacls2 = objc.metaclass(cls2)
	local obj2 = cls2:new()

	function metacls2:description(callsuper) --override the class method
		return objc.NSString:alloc():initWithUTF8String(callsuper(self):UTF8String() .. '2')
	end

	function cls2:description(callsuper) --override the instance method
		return objc.NSString:alloc():initWithUTF8String(callsuper(self):UTF8String() .. '2')
	end

	assert(cls2:description():UTF8String() == classdesc..'2') --class method was overriden
	assert(obj2:description():UTF8String() == instdesc..'2') --instance method was overriden and it's different
end

function test.ivars()
	local obj = objc.NSDocInfo:new()

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
end

function test.properties()
	local pr = objc.NSProgress:progressWithTotalUnitCount(123)
	assert(pr.totalUnitCount == 123) --as initialized
	pr.totalUnitCount = 321 --read/write property
	assert(pr.totalUnitCount == 321)
	assert(not pcall(function() pr.indeterminate = 1 end)) --attempt to set read/only property
	assert(pr.indeterminate == 0)
end

function test.blocks()
	local s = objc.NSString:alloc():initWithUTF8String('line1\nline2\nline3')
	local t = {}
	local block, callback = objc.block(function(line, pstop)
		t[#t+1] = line:UTF8String()
		if #t == 2 then --stop at line 2
			pstop[0] = 1
		end
	end, 'v@^B')
	s:enumerateLinesUsingBlock(block)
	callback:free()
	assert(#t == 2)
	assert(t[1] == 'line1')
	assert(t[2] == 'line2')
end

function test.tolua()
	local n = objc.toobj(123.5)
	assert(objc.isa(n, 'NSNumber'))
	assert(objc.tolua(n) == 123.5)

	local s = objc.toobj'hello'
	assert(objc.isa(s, 'NSString'))
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
end

function test.args()
	local s = objc.NSString:alloc():initWithUTF8String'\xE2\x82\xAC' --euro symbol
	--return string
	assert(s:UTF8String() == '\xE2\x82\xAC')
	--return boolean (doesn't work for methods)
	assert(s:isAbsolutePath() == 0)
	--return null
	assert(type(s:cStringUsingEncoding(objc.NSASCIIStringEncoding)) == 'nil')
	--selector arg
	assert(objc.NSObject:respondsToSelector'methodForSelector:' == 1)
	--class arg
	assert(objc.NSArray:isSubclassOfClass'NSObject' == 1)
	assert(objc.NSArray:isSubclassOfClass'XXX' == 0)
	--string arg
	assert(objc.NSString:alloc():initWithString('hey'):UTF8String() == 'hey')
	--table arg for array
	local a = objc.NSArray:alloc():initWithArray{6,25,5}
	assert(a:objectAtIndex(1):doubleValue() == 25)
	--table arg for dictionary
	local d = objc.NSDictionary:alloc():initWithDictionary{a=5,b=7}
	assert(d:valueForKey('b'):doubleValue() == 7)
end

function eyetest.window()
	objc.load'AppKit'

	local pool = objc.NSAutoreleasePool:new()
	local NSApp = objc.class('NSApp', 'NSApplication <NSApplicationDelegate>')

	--we need to add methods to the class before creating any objects!
	--note: NSApplicationDelegate is an informal protocol brought from bridgesupport.

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
	--note: NSWindowDelegate is a formal protocol brought from the runtime.

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

-- inspection ------------------------------------------------------------------------------------------------------------

local function load_many_frameworks()
	objc.load'System'
	objc.load'Carbon'
	objc.load'AppKit'
	objc.load'IOKit'
	objc.load'AVFoundation'
	objc.load'WebKit'
end

function eyetest.inspect_classes()
	load_many_frameworks()
	objc.inspect.classes()
end

function eyetest.inspect_protocols()
	load_many_frameworks()
	objc.inspect.protocols()
end

function eyetest.inspect_class_properties(cls)
	load_many_frameworks()
	objc.inspect.class_properties(cls)
end

function eyetest.inspect_protocol_properties(proto)
	load_many_frameworks()
	objc.inspect.protocol_properties(proto)
end

local function req(s)
	return s and s ~= '' and s or nil
end

function eyetest.inspect_class_methods(cls, inst)
	load_many_frameworks()
	objc.inspect.class_methods(req(cls), inst == 'inst')
end

function eyetest.inspect_protocol_methods(proto, inst, required)
	load_many_frameworks()
	objc.inspect.protocol_methods(req(proto), inst == 'inst', required == 'required')
end

function eyetest.inspect_class_ivars(cls)
	load_many_frameworks()
	objc.inspect.class_ivars(req(cls))
end

function eyetest.inspect_class(cls)
	load_many_frameworks()
	objc.inspect.class(cls)
end

function eyetest.inspect_protocol(proto)
	load_many_frameworks()
	objc.inspect.protocol(proto)
end

function eyetest.inspect_find(patt)
	load_many_frameworks()
	objc.inspect.find(patt)
end

--------------

local function test_all(tests, ...)
	for k,v in glue.sortedpairs(tests) do
		if k ~= 'all' then
			print(k)
			hr()
			tests[k](...)
		end
	end
end

function test.all(...)
	test_all(test)
end

--cmdline interface

local test_name = ...

if not test_name then
	print('Usage: '..luajit..' '..arg[0]..' <test>')
	print'Available tests:'
	for k in glue.sortedpairs(test) do
		print('', k)
	end
	print'Available eye tests:'
	for k in glue.sortedpairs(eyetest) do
		print('', k)
	end
else
	local test = test[test_name] or eyetest[test_name]
	if not test then
		print('Invalid test '..tostring(test_name))
		os.exit(1)
	end
	test(select(2, ...))
	print'ok'
end

