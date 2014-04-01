local ffi = require'ffi'
assert(ffi.abi'32bit', 'use luajit32 with this')
local cairo = require'cairo'
require'cairo_quartz'
local objc = require'objc'
--objc.debug = true
local bs = require'objc.BridgeSupport'
bs.loadFramework'Foundation'
bs.loadFramework'AppKit'
bs.loadFramework'CoreGraphics'
setmetatable(_G, {__index=objc})

local appName = NSStr(arg[0] or '')

local NSApp = NSApplication:sharedApplication()
NSApp:setActivationPolicy(bs.NSApplicationActivationPolicyRegular) --normal app with dock and menu bar

--create a window
local mainWindow = NSWindow:alloc():
		initWithContentRect_styleMask_backing_defer({{0, 0}, {800, 400}}, 
			bit.bor(
				bs.NSTitledWindowMask, 
				bs.NSClosableWindowMask,
				bs.NSMiniaturizableWindowMask,
				bs.NSResizableWindowMask),
			bs.NSBackingStoreBuffered, 
			false)
mainWindow:center()
mainWindow:setTitle(appName)

--subclass a view
local CairoView = createClass(NSView, 'CairoView', {})

local function on_render(cr)
	cr:set_source_rgba(1,0,0,1)
	cr:paint()
end

local q_context
local q_surface
local q_cx
local p_surface
local p_cx
addMethod(CairoView, SEL("drawRect:"), function(self, sel, x, y, w, h)
	local q_cr = NSGraphicsContext:currentContext():graphicsPort()
	q_cr = ffi.cast('CGContextRef', q_cr)
	if q_context ~= q_cr or p_surface:get_image_width() ~= w or p_surface:get_image_height() ~= h then
		if q_surface then
			q_cx:free()
			q_surface:free()
			p_cx:free()
			p_surface:free()
		end
		q_surface = cairo.cairo_quartz_surface_create_for_cg_context(q_cr, w, h)
		q_context = q_cr
		q_cx = q_surface:create_context()
		p_surface = cairo.cairo_image_surface_create(cairo.CAIRO_FORMAT_RGB24, w, h)
		q_cx:set_source_surface(p_surface, 0, 0)
		p_cx = p_surface:create_context()
	end
	on_render(p_cx)
	q_cx:paint()
end, 'v@:ffff')

addMethod(CairoView, SEL('mouseMoved:'), function(self, sel, event)
	local loc = event:locationInWindow()
	print(loc.x, loc.y)
end, 'v@:@')

addMethod(CairoView, SEL("mouseDown:"), function(self, sel, event)
	local loc = event:locationInWindow()
	print(loc.x, loc.y)
end, "v@:@")

addMethod(CairoView, SEL("rightMouseDown:"), function(self, sel, event)
	local loc = event:locationInWindow()
	print(loc.x, loc.y)
end, "v@:@")

addMethod(CairoView, SEL('isFlipped'), function(self, sel)
	return true
end, 'B@:')

addMethod(CairoView, SEL('viewDidEndLiveResize'), function(self, sel)
	print'here'
end, 'v@:')

--create a cairoview
local cview = CairoView:alloc():init()
mainWindow:setContentView(cview)

--create a tracking area
local opts = bit.bor(bs.NSTrackingActiveAlways, bs.NSTrackingInVisibleRect, bs.NSTrackingMouseEnteredAndExited, bs.NSTrackingMouseMoved)
local f = mainWindow:frame(); print(f.origin.x, f.origin.y, f.size.width, f.size.height)
local area = NSTrackingArea:alloc():initWithRect_options_owner_userInfo(f, opts, cview, nil)
cview:addTrackingArea(area)

--create the menubar
local menuBar = NSMenu:alloc():init()
local appMenuItem = NSMenuItem:alloc():init()
menuBar:addItem(appMenuItem)
NSApp:setMainMenu(menuBar)

--create the app menu
local appMenu = NSMenu:alloc():init()
appMenu:setTitle(appName)
quitMenuItem = NSMenuItem:alloc():initWithTitle_action_keyEquivalent(NSStr'Quit', SEL('terminate:'), NSStr('q'))
appMenu:addItem(quitMenuItem)
appMenuItem:setSubmenu(appMenu)

--bring the app out
mainWindow:makeKeyAndOrderFront(NSApp)
NSApp:activateIgnoringOtherApps(true)
NSApp:run()

