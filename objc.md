---
project:   objc
platforms: osx32, osx64
tagline:   Objective-C bridge
---

## `local objc = require'objc'`

Objecive-C runtime and BridgeSupport binding.

## Quick Tour

### Loading Frameworks

~~~{.lua}
--load a framework by name; `objc.searchpaths` says where the frameworks are. you can also use full paths.
--classes and protocols are loaded, but also C constants, enums, functions, structs and even macros.
objc.load'Foundation'
~~~

### Creating and using Objects

~~~{.lua}
--instantiate a class. the resulting object is retained and released on gc.
--you can call `release()` on it too, for a more speedy destruction.
local str = objc.NSString:alloc():initWithUTF8String'wazza'

--call methods with multiple arguments using underscores for ':'. last underscore is optional.
--C constants, enums and functions are in the objc namespace too.
local result = str:compare_options(otherStr, objc.NSLiteralSearch)
~~~

### Subclassing

~~~{.lua}
--create a derived class. when creating a class, say which protocols you wish it conforms to,
--so that you don't have to deal with type encodings when implementing its methods.
objc.class('NSMainWindow', 'NSWindw <NSWindowDelegate>')

--add methods to your class. the selector `windowWillClose` is from the `NSWindowDelegate` protocol
--so its type encoding is inferred from the protocol definition.
function objc.NSMainWindow:windowWillClose(notification)
	...
end

--override existing methods. when overriding an existing method, arg#1 is sugar for calling the supermethod.
function objc.NSMainWindow:update(callsuper)
	...
	return callsuper(self)
end

~~~

### Adding Lua Variables

~~~{.lua}
--add Lua variables to your objects - their lifetime is tied to the lifetime of the object.
--you can also add class variables - they will be accessible through the objects too.
objc.NSObject.myClassVar = 'I can live forever'
local obj = objc.NSObject:new()
obj.myInstanceVar = 'I live while obj lives'
obj.myClassVar = 42 --change the class var (same value for all objects)
~~~

### Accessing Properties & IVars

~~~{.lua}
--get and set class and instance properties using the dot notation.
local pr = objc.NSProgress:progressWithTotalUnitCount(123)
print(pr.totalUnitCount) --prints 123
pr.totalUnitCount = 321  --sets it

--get and set ivars using the dot notation.
local obj = objc.NSDocInfo:new()
obj.time = 123
print(obj.time) --prints 123
~~~

### Creating and using Blocks

~~~{.lua}
--blocks are created automatically when passing a Lua function where a block is expected.
--their lifetime is auto-managed, for both synchronous and asynchronous methods.
local str = objc.NSString:alloc():initWithUTF8String'line1\nline2\nline3'
str:enumerateLinesUsingBlock(function(line, stop)
	print(line:UTF8String()) --'char *' return values are also converted to Lua strings automatically
end)

--however, blocks are slow to create and use ffi callbacks which are very limited in number.
--create your blocks outside loops if possible, or call `collectgarbage()` every few hundred iterations.

--create a block with its type signature inferred from usage.
--in this case, its type is that of arg#1 to NSString's `enumerateLinesUsingBlock` method.
local block = objc.toarg(objc.NSString, 'enumerateLinesUsingBlock', 1, function(line, stop)
	print(line:UTF8String())
end)
str:enumerateLinesUsingBlock(block)

--create a block with its method type encoding given manaully.
--for type encodings see:
--   https://code.google.com/p/jscocoa/wiki/MethodEncoding
--   https://developer.apple.com/library/mac/documentation/Cocoa/Conceptual/ObjCRuntimeGuide/Articles/ocrtTypeEncodings.html
local block = objc.block(function(line, stop)
	print(line:UTF8String())
end, 'v@^B'}) --retval is 'v' (void), line is '@' (object), stop is '^B' (pointer to BOOL)
str:enumerateLinesUsingBlock(block)
~~~

## API Refrence

----------------------------------------------------------- --------------------------------------------------------------
__frameworks__

`objc.load(name|path[, option])`										load a framework given its name or its full path \
																				option 'notypes': don't load bridgesupport file

`objc.searchpaths = {path1, ...}`									search paths for frameworks

__selectors__

`objc.SEL(s|sel) -> sel`												create/find a selector by name

`sel:name() -> s`															selector's name (same as tostring(sel))

__classes__

`objc.class'NSString' -> cls`											class by name (same as objc.NSString)

`objc.class(obj) -> cls`												class of instance

`objc.class('Foo', 'SuperFoo <Protocol1, ...>') -> cls`		create a class

`objc.class('Foo', 'SuperFoo', 'Protocol1', ...) -> cls`		create a class (alternative way)

----------------------------------------------------------- --------------------------------------------------------------


## Reflection API

----------------------------------------------------------- --------------------------------------------------------------
__protocols__
`objc.protocols() -> clist<proto>`									list all loaded protocols
`objc.protocol(name|proto) -> proto`								get a protocol by name
`proto:name() -> s`														protocol name
`proto:protocols() -> clist<proto>`									inherited protocols
`proto:properties() -> clist<prop>`									get properties (inherited ones not included)
`proto:property(proto, name, required, readonly) -> prop`	find a property
`proto:method_types(proto, inst, req) -> {name=types}`		get methods and their types
`proto:method_type(proto, sel, inst, req) -> {name=types}`	find a method
`proto:search(f, ...) -> ret`											call f(proto, ...) for proto and all superprotocols recursively. stops when f() returns something and returns that value.
__classes__
`objc.classes() -> clist<cls>`										list all loaded classes
__properties__
`prop:name()`
`prop:getter() -> s`
`prop:setter() -> s`
----------------------------------------------------------- --------------------------------------------------------------


## Debugging and Fine Tuning

----------------------------------------------------------- --------------------------------------------------------------
__logging__
objc.debug.errors (true)												log errors to stderr
objc.debug.methodtypes (false)										log method ctype parsing
objc.debug.release (false)												log relases with refcount
objc.debug.retain (false)												log retains with refcount
objc.debug.load (false)													log loaded frameworks
objc.debug.cdefs (false)												print cdefs to stdout (then you can grab them and make your own static cdef headers)
__stats__
objc.debug.stats.errors										         number of cdef errors
objc.debug.stats.cdefs													number of cdefs without error
objc.debug.stats.redef													number of incompatible redefines (if objc.debug.redef == true)
__ctype renaming__
objc.debug.rename.string.foo = bar									load a string constant under a different name
objc.debug.rename.enum.foo = bar										load an enum under a different name
objc.debug.rename.typedef.foo = bar									load a type under a different name
objc.debug.rename.const.foo = bar									load a const under a different name
objc.debug.rename.function.foo = bar								load a global function under a different name
__bridgesupport options__
objc.debug.loaddeps (false)											load dependencies per bridgesupport file (usually too many to be useful)
objc.debug.lazyfuncs (true)											cdef functions on the first call
objc.debug.redef (false)												check incompatible redefinition attempts (makes parsing slower)
__framework loading fine tuning__
objc.debug.loadtypes (true)											load bridgesupport files
`objc.find_framework(name|path) -> path, name`					find a framework in `objc.searchpaths`
`objc.load_bridgesupport(path, loaddeps)`							load a bridgesupport file
`objc.debug.loaded -> {name = true}`								loaded frameworks
`objc.debug.loaded_bs -> {name = true}`							frameworks for which bridgesupport was loaded too
----------------------------------------------------------- --------------------------------------------------------------

## Quick Tutorial

...


