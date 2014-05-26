---
project:   objc
platforms: osx32, osx64
tagline:   Objective-C bridge
---

## `local objc = require'objc'`

Objecive-C runtime and BridgeSupport binding.

## Basic API

----------------------------------------------------------- --------------------------------------------------------------
__frameworks__

`objc.load(name|path[, option])`										load a framework given its name or full path \
																				option 'notypes': don't load bridgesupport file

`objc.searchpaths = {path1, ...}`									search paths for frameworks

__selectors__

`objc.selector(s|sel) -> sel`											create/find a selector by name

`sel:name() -> s`															selector's name (same as tostring(sel))

__classes__

`objc.class(name[, superclass[, proto, ...]]) -> cls`			create a class

`objc.class(name[, def]) -> cls`										create a class; def = 'ClassFoo <ProtocolBar, ProtocolBaz>`
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

