-- Ported from LuaCocoa's MinimalAppKit example
-- Creates a quit menu item and a window. Very minimal indeed.

package.path = package.path .. ';?/init.lua;../?.lua'
local ffi = require'ffi'
local objc = require("objc")
local cairo = require'cairo'
--objc.debug = true
local bs = require("objc.BridgeSupport")
bs.loadFramework("Foundation", true)
bs.loadFramework("AppKit", true)
bs.loadFramework("ApplicationServices")
setmetatable(_G, {__index=objc})

local appName = NSStr("hello!!!") --NSProcessInfo:processInfo():processName()

local NSApp = NSApplication:sharedApplication()
NSApp:setActivationPolicy(NSApplicationActivationPolicyRegular)

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
local mainWindow = NSWindow:alloc():initWithContentRect_styleMask_backing_defer({{0, 0}, {200, 200}}, NSTitledWindowMask, NSBackingStoreBuffered, false)
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

addMethod(CairoView, SEL("drawRect:"), function(self, sel, rect)
  local cx = NSGraphicsContext:currentContext()
  local cr = cx:graphicsPort()
  col = ffi.new('CGFloat[4]', 0.98, 0.9, 0.88, 1.0)
  --CGContextSetFillColor (cr, col)
  --CGContextFillRect (cr, CGRectMake (0.0, 0.0, rect.size.width, rect.size.height))
end, 'v@:@')

local cview = CairoView:alloc():init()
mainWindow:setContentView(cview)

-- Bring the app out
NSApp:activateIgnoringOtherApps(true)
NSApp:run()

