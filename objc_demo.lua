-- Ported from LuaCocoa's MinimalAppKit example
-- Creates a quit menu item and a window. Very minimal indeed.

local ffi = require'ffi'
assert(ffi.abi'32bit', 'use luajit32 with this')
local objc = require("objc")
local cairo = require'cairo'
require'cairo_quartz'
--objc.debug = true
local bs = require("objc.BridgeSupport")
bs.loadFramework("Foundation")
bs.loadFramework("AppKit")
bs.loadFramework("ApplicationServices")
bs.loadFramework("CoreGraphics")
setmetatable(_G, {__index=objc})

local appName = NSStr("hello!!!") --NSProcessInfo:processInfo():processName()

local NSApp = NSApplication:sharedApplication()
NSApp:setActivationPolicy(bs.NSApplicationActivationPolicyRegular)

-- Create the menubar
local menuBar = NSMenu:alloc():init()
local appMenuItem = NSMenuItem:alloc():init()
menuBar:addItem(appMenuItem)
NSApp:setMainMenu(menuBar)

-- Create a file menu
local fileMenuItem = NSMenuItem:alloc():init()
menuBar:addItem(fileMenuItem)
local fileMenu = NSMenu:alloc():init()
fileMenuItem:setSubmenu(fileMenu)
fileMenu:setTitle(appName)

-- Create the App menu
local appMenu = NSMenu:alloc():init()
appMenu:setTitle(appName)
local quitTitle = "Quit " .. tostring(appName)
quitMenuItem = NSMenuItem:alloc():initWithTitle_action_keyEquivalent(NSStr(quitTitle), SEL("terminate:"), NSStr("q"))
appMenu:addItem(quitMenuItem)
appMenuItem:setSubmenu(appMenu)

-- Create a window
local mainWindow = NSWindow:alloc():initWithContentRect_styleMask_backing_defer({{0, 0}, {800, 400}}, bs.NSTitledWindowMask, bs.NSBackingStoreBuffered, false)
mainWindow:cascadeTopLeftFromPoint({20,20})
mainWindow:setTitle(appName)
mainWindow:makeKeyAndOrderFront(NSApp)

--https://developer.apple.com/library/mac/#documentation/Cocoa/Conceptual/ObjCRuntimeGuide/Articles/ocrtTypeEncodings.html
local CairoView = createClass(NSView, "CairoView", {})

--[[
-- Creates an init method returning an object(@) and taking as arguments an object(@) and a selector(:)
-- All methods must take self and selector as their first two arguments
addMethod(MyClass, SEL("init"), function(self, sel)
    print("Creating an instance of", self:class())
    setIvar(self, "anIvar", 123)
    return callSuper(self, sel)
end, "@@:")


-- Add a getter for 'ivar'
addMethod(MyClass, SEL("ivar"), function(self, sel)
    return getIvar(self, "ivar")
end, "i@:")

-- Add a setter for 'ivar'
addMethod(MyClass, SEL("setIvar:"), function(self, sel, anIvar)
    setIvar(self, "ivar", anIvar)
end, "v@:i")
]]

local surface
local context
local cx 
addMethod(CairoView, SEL("drawRect:"), function(self, sel, x, y, w, h)
  local cr = NSGraphicsContext:currentContext():graphicsPort()
  cr = ffi.cast('CGContextRef', cr)
  --local rect = bs.CGRectMake(100, 100, 200, 200)
  --bs.CGContextFillRect (cr, rect)
  if context ~= cr then
      if surface then
          cx:free()
          surface:free()
      end
      surface = cairo.cairo_quartz_surface_create_for_cg_context(cr, w, h)
      context = cr
      cx = surface:create_context()
  end
  cx:set_source_rgba(1,0,0,0.5)
  cx:paint()
end, 'v@:ffff')

local cview = CairoView:alloc():init()
mainWindow:setContentView(cview)

-- Bring the app out
NSApp:activateIgnoringOtherApps(true)
NSApp:run()

