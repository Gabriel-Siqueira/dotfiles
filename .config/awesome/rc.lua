-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
-- layout symilar to i3
local treesome = require("treesome")
-- for the widgets
local vicious = require("vicious")
-- freedesktop (used for menu)
local freedesktop = require("freedesktop")
-- lain (new widgets)
local lain = require("lain")
-- keydoc (key maps infos)
local keydoc = require("keydoc")
-- scratch (drop down)
local scratch = require("scratch")

-- {{{ ***************      Functions       ***************

quit = function()
  local scr = mouse.screen
  awful.prompt.run({prompt = "Quit (type 'yes' to confirm)? "},
  mypromptbox[scr].widget,
  function (t)
    if string.lower(t) == 'yes' then
			awesome.quit()
    end
  end,
  function (t, p, n)
    return awful.completion.generic(t, p, n, {'no', 'NO', 'yes', 'YES'})
  end)
end

system_lock = function ()
  awful.util.spawn("i3lock -i /home/gabriel/Pictures/lock_und_dm/rsz_dragon.png")
end

system_suspend = function ()
  awful.util.spawn("systemctl suspend")
end

system_hibernate = function ()
  local scr = mouse.screen
  awful.prompt.run({prompt = "Hibernate (type 'yes' to confirm)? "},
  mypromptbox[scr].widget,
  function (t)
    if string.lower(t) == 'yes' then
      awful.util.spawn("systemctl hibernate")
    end
  end,
  function (t, p, n)
    return awful.completion.generic(t, p, n, {'no', 'NO', 'yes', 'YES'})
  end)
end

system_hybrid_sleep = function ()
  local scr = mouse.screen
  awful.prompt.run({prompt = "Hybrid Sleep (type 'yes' to confirm)? "},
  mypromptbox[scr].widget,
  function (t)
    if string.lower(t) == 'yes' then
      awful.util.spawn("systemctl hybrid-sleep")
    end
  end,
  function (t, p, n)
    return awful.completion.generic(t, p, n, {'no', 'NO', 'yes', 'YES'})
  end)
end

system_reboot = function ()
  local scr = mouse.screen
  awful.prompt.run({prompt = "Reboot (type 'yes' to confirm)? "},
  mypromptbox[scr].widget,
  function (t)
    if string.lower(t) == 'yes' then
      awful.util.spawn("systemctl reboot")
    end
  end,
  function (t, p, n)
    return awful.completion.generic(t, p, n, {'no', 'NO', 'yes', 'YES'})
  end)
end

system_power_off = function ()
  local scr = mouse.screen
  awful.prompt.run({prompt = "Power Off (type 'yes' to confirm)? "},
  mypromptbox[scr].widget,
  function (t)
    if string.lower(t) == 'yes' then
      awful.util.spawn("systemctl poweroff")
    end
  end,
  function (t, p, n)
    return awful.completion.generic(t, p, n, {'no', 'NO', 'yes', 'YES'})
  end)
end

-- }}}

-- {{{ ***************      Auto-start      ***************

awful.util.spawn_with_shell("thunderbird")
awful.util.spawn_with_shell("amor")
awful.util.spawn_with_shell("dropbox")
awful.util.spawn_with_shell("compton -b")
awful.util.spawn_with_shell("xfce4-power-maneger")

-- }}}

-- {{{ ***************    Error handling    ***************

-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end

-- }}}

-- {{{ *************** Variable definitions ***************

-- some system info
home = os.getenv("HOME")
confdir = home .. "/.config/awesome"
themes = confdir .. "/themes"
active_theme = themes .. "/zenburn"
language = string.gsub(os.getenv("LANG"),".utf8","")

-- Themes define colours, icons, font and wallpapers.
beautiful.init("/home/gabriel/.config/awesome/themes/zenburn/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "terminator"
editor = os.getenv("EDITOR") or "vim"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
modkey = "Mod4"
altkey = "Mod1"

-- settings on lain layouts
lain.layout.termfair.nmaster = 3

-- Table of layouts
local layouts =
{
		treesome,
    awful.layout.suit.max,
    awful.layout.suit.floating,
    -- awful.layout.suit.fair,
    -- awful.layout.suit.fair.horizontal,
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
    -- awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier,
		lain.layout.termfair,
		lain.layout.cascade,
		lain.layout.centerwork
}

-- }}}


-- {{{ ***************      Wallpaper       ***************

-- Random Wallpapers

-- Get the list of files from a directory. Must be all images or folders and non-empty. 
function scanDir(directory)
	local i, fileList, popen = 0, {}, io.popen
	for filename in popen([[find "]] ..directory.. [[" -type f]]):lines() do
	    i = i + 1
	    fileList[i] = filename
  end
	return fileList
end

-- configuration
wp_timeout  = 900
wallpaperList = scanDir("/home/gabriel/Pictures/mywallpaper/favorites/")
 
-- setup the timer
wp_timer = timer { timeout = wp_timeout }
wp_timer:connect_signal("timeout", function()
 
  -- set wallpaper to current index for all screens
  for s = 1, screen.count() do
		gears.wallpaper.maximized(wallpaperList[math.random(1, #wallpaperList)], s, true)
  end
	
  -- stop the timer (we don't need multiple instances running at the same time)
  wp_timer:stop()
 
  --restart the timer
  wp_timer.timeout = wp_timeout
  wp_timer:start()
end)

-- initial wallpaper
gears.wallpaper.maximized(wallpaperList[math.random(1, #wallpaperList)], s, true)
-- initial start when rc.lua is first run
wp_timer:start()

-- }}}

-- {{{ ***************         Tags         ***************

-- Define a tag table which hold all screen tags.
tags = {
	 names = { "standard", "aux➊", "aux➋", ".", "..", "...", "Game ", "VM ", "email " },
	 layout = { layouts[1], layouts[1], layouts[1], layouts[1], layouts[1], layouts[1], layouts[1], layouts[1], layouts[2]}
}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag(tags.names, s, tags.layout)
end

-- }}}

-- {{{ ***************         Menu         ***************

menu_icon = "/usr/share/icons/Dowloaded/arch-linux-icon.png"
menu_items = freedesktop.menu.new()
myawesomemenu = {
   { "manual", terminal .. " -e man awesome", freedesktop.utils.lookup_icon({ icon = 'help' }) },
   { "edit config", editor_cmd .. " " .. awful.util.getdir("config") .. "/rc.lua", freedesktop.utils.lookup_icon({ icon = 'package_settings' }) },
   { "restart", awesome.restart, freedesktop.utils.lookup_icon({ icon = 'gtk-refresh' }) },
   { "quit", awesome.quit, freedesktop.utils.lookup_icon({ icon = 'gtk-quit' }) }
}
table.insert(menu_items, { "awesome", myawesomemenu, beautiful.awesome_icon })
table.insert(menu_items, { "open terminal", terminal, freedesktop.utils.lookup_icon({icon = 'terminal'}) })

mymainmenu = awful.menu({ items = menu_items })

mylauncher = awful.widget.launcher({ image = menu_icon, menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it

-- }}}

-- {{{ ***************         Wibox        ***************

-- {{{ Battery widget
batwidget = wibox.widget.textbox()
vicious.register(batwidget, vicious.widgets.bat,
								 function (widget, args)
										if args[1] == "↯" then return "<span foreground=\"#00ff00\"></span> " .. args[2] .. "% | "
										elseif args[1] == "+" and args[2] > 30 then return "<span foreground=\"#00ff00\"></span> " .. args[2] .. "% | "
										elseif args[1] == "+" then return "<span foreground=\"#ff0000\"></span> " .. args[2] .. "% | "
										elseif args[2] > 30 then return "<span foreground=\"#00ff00\">▇</span> " .. args[2] .. "% | "
										else return "<span foreground=\"#ff0000\">▄</span> " .. args[2] .. "% | "
										end
									end
, 13, "BAT1")
-- }}}

-- {{{ Memory widget
memwidget = wibox.widget.textbox()
vicious.register(memwidget, vicious.widgets.mem, "<span foreground=\"#cc00cc\"></span> $2 MB/$3 MB | ", 13)
-- }}}
-- {{{ Disk Space widget
diskwidget = wibox.widget.textbox()
vicious.register(diskwidget, vicious.widgets.fs, " | <span foreground=\"#cc00cc\"></span> ${/ used_gb} GB / ${/ size_gb} GB | ", 13)
diskwidget:buttons(awful.util.table.join(
	awful.button({ }, 1, function () awful.util.spawn("thunar") end)
))
-- }}}
-- {{{ Wifi widget
wifiwidget = wibox.widget.textbox()
vicious.register(wifiwidget, vicious.widgets.wifi,
								 function (widget, args)
										if args['{linp}'] ~= 0 then return "<span foreground=\"#00ff00\"></span> " .. args['{ssid}'] .. " - " .. args['{linp}'] .. "% | "
										else return "<span foreground=\"#999966\"></span> | "
										end
								end
, 13, "wlp2s0")
wifiwidget:buttons(awful.util.table.join(
	awful.button({ }, 1, function () awful.util.spawn("wicd-gtk") end)
))
-- }}}

-- {{{ Packages updates widget
pkgwidget = wibox.widget.textbox()
vicious.register(pkgwidget, vicious.widgets.pkg, "<span foreground=\"#ff6600\"></span> $1 | ", 13, "Arch")
-- }}}
-- {{{ Volume widget
volwidget = wibox.widget.textbox()
vicious.register(volwidget, vicious.widgets.volume,
								 function (widget, args)
										if   args[2] == "♫" then return "<span foreground=\"#ffff00\"></span> " ..  args[1] .. "% | "
										else return "<span foreground=\"#999966\"></span> | "
										end
								 end
								 , 13, "Master")
volwidget:buttons(awful.util.table.join(
	awful.button({ }, 1, function () awful.util.spawn("pavucontrol") end)
))
-- }}}
-- {{{ Date and clock widget
datewidget = wibox.widget.textbox()
vicious.register(datewidget, vicious.widgets.date, "<span foreground=\"#ff6600\"></span> %d/%m/%Y | <span foreground=\"#0066ff\"></span> %r | ")
lain.widgets.calendar:attach(datewidget)
-- }}}
-- Create a wibox for each screen and add it
-- {{{ declare items
mywibox= {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({
                                                      theme = { width = 250 }
                                                  })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))
-- }}}

-- {{{ top bar
for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

    -- Create a tasklist widget
    -- mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s })

		-- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(mylauncher)
    left_layout:add(mytaglist[s])
    left_layout:add(mypromptbox[s])

    -- Widgets that are aligned to the right
    local right_layout = wibox.layout.fixed.horizontal()
    right_layout:add(diskwidget)
		right_layout:add(memwidget)
    right_layout:add(pkgwidget)
    right_layout:add(wifiwidget)
    right_layout:add(volwidget)
    right_layout:add(batwidget)
    right_layout:add(datewidget)
    if s == 1 then right_layout:add(wibox.widget.systray()) end

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)

    mywibox[s]:set_widget(layout)
end
-- }}}

-- {{{ bottom bar
for s = 1, screen.count() do
    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "bottom", screen = s })

    -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(mylayoutbox[s])
		
    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_middle(mytasklist[s])
    layout:set_left(left_layout)

    mywibox[s]:set_widget(layout)
end
-- }}}

-- {{{ Other aditons on each screen
promptbox = {}
for s = 1, screen.count() do
    -- Create a promptbox for each screen
    promptbox[s] = awful.widget.prompt()
end
-- }}}

-- }}}

-- {{{ ***************    Mouse bindings    ***************

root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))

-- }}}

-- {{{ ***************     key bindings     ***************

-- {{{ define global keys
globalkeys = awful.util.table.join(
		keydoc.group("Tags"),
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       , "go ot next tag"),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       , "go to previl tag"),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore, "go to last used tag"),

		keydoc.group("Window focus"),
    awful.key({ modkey,           }, "l",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end, "change focus (inc)"),
    awful.key({ modkey,           }, "h",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end,  "change focus (dec)"),

		keydoc.group("Menu"),
    awful.key({ modkey,           }, "w", function () mymainmenu:show() end, "show menu"),

    -- Layout manipulation
		keydoc.group("Layout manipulation"),
    awful.key({ modkey, "Shift"   }, "l", function () awful.client.swap.byidx(  1)    end, "Swap with next window"),
    awful.key({ modkey, "Shift"   }, "h", function () awful.client.swap.byidx( -1)    end, "Swap with previous window"),
    awful.key({ modkey, "Control" }, "l", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "h", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto, "jump to urgent client"),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end, "last previous focus"),

    awful.key({ modkey,           }, "j",     function () awful.tag.incmwfact( 0.05)    end, "increase focus client size"),
    awful.key({ modkey,           }, "k",     function () awful.tag.incmwfact(-0.05)    end, "decrease focus client size"),
    awful.key({ modkey, "Shift"   }, "j",     function () awful.tag.incnmaster( 1)      end, "increase number of masters clients"),
    awful.key({ modkey, "Shift"   }, "k",     function () awful.tag.incnmaster(-1)      end, "decrease number of masters clients"),
    awful.key({ modkey, "Control" }, "j",     function () awful.tag.incncol( 1)         end, "increase size of column"),
    awful.key({ modkey, "Control" }, "k",     function () awful.tag.incncol(-1)         end, "decrease size of column"),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end, "next layout"),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end, "prev layout"),

    awful.key({ modkey, "Control" }, "n", awful.client.restore, "restore minimize clients"),

		-- Treesome
		awful.key({ modkey }, "v", treesome.vertical, "treesome next split vertical (ç horisontal)"),
    awful.key({ modkey }, "ç", treesome.horizontal),

		-- Standard program
		keydoc.group("Misc"),
    awful.key({ modkey, "Shift"   }, "Return", function () awful.util.spawn(terminal) end, "Spawn a terminal"),
    awful.key({ modkey, "Shift"   }, "r", awesome.restart, "restart"),
    awful.key({ modkey, "Shift"   }, "e", quit, "quit"),
    awful.key({ modkey, altkey    }, "l", system_lock, "lock"),
		
		-- scratch
		awful.key({ modkey }, "Return",
			 function ()
					scratch.drop("termite", "top", "center", 0.7, 0.4)
			 end, "Drop down terminal"),

    -- Prompt
    awful.key({ modkey }, "r", function () mypromptbox[mouse.screen]:run() end, "run aplication"),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end, "run lua code"),
    -- Menubar
    awful.key({ modkey }, "d", function() menubar.show() end, "menu bar"),

		-- keydoc
		awful.key({ modkey, }, "F1", keydoc.display),
		 
		-- Volume
		awful.key({ }, "XF86AudioRaiseVolume", function ()
       awful.util.spawn("amixer set Master 5%+") end),
   awful.key({ }, "XF86AudioLowerVolume", function ()
       awful.util.spawn("amixer set Master 5%-") end),
   awful.key({ }, "XF86AudioMute", function ()
       awful.util.spawn("amixer sset Master toggle") end),

	 -- Brightness
   awful.key({ }, "XF86MonBrightnessDown", function ()
       awful.util.spawn("xbacklight -dec 5") end),
   awful.key({ }, "XF86MonBrightnessUp", function ()
       awful.util.spawn("xbacklight -inc 5") end),
	 
	 -- Screenshot
	 awful.key({ }, "Print", function () awful.util.spawn("xfce4-screenshot") end)
)
-- }}}


-- {{{ define clientkeys keys
clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "q",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)
-- }}}


-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
-- {{{ 
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        local tag = awful.tag.gettags(screen)[i]
                        if tag then
                           awful.tag.viewonly(tag)
                        end
                  end),
        -- Toggle tag.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      local tag = awful.tag.gettags(screen)[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.movetotag(tag)
                          end
                     end
                  end),
        -- Toggle tag.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.toggletag(tag)
                          end
                      end
                  end))
end
-- }}}


clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)

-- }}}

-- {{{ ***************         Rules        ***************

-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
										 size_hints_honor = false,
                     buttons = clientbuttons } },
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "pinentry" },
      properties = { floating = true } },
    { rule = { class = "gimp" },
      properties = { floating = true } },
    -- Set Thunderbird to always map on tags number 2 of screen 1.
		{ rule = { class = "Thunderbird" },
		properties = { tag = tags[1][9] } },
		{ rule = { class = "VirtualBox" },
		properties = { tag = tags[1][8] } },
		{ rule = { class = "Steam" },
		properties = { tag = tags[1][7] } },
		{ rule = { name = "PlayOnLinux" },
		properties = { tag = tags[1][7] } },
		{ rule = { class = "Minetest" },
		properties = { tag = tags[1][7] } },
		{ rule = { instance = "plugin-container" },
		properties = { floating = true } },
}

-- }}}

-- {{{ ***************        Signals       ***************

-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    -- Enable sloppy focus
    c:connect_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end

    local titlebars_enabled = false
    if titlebars_enabled and (c.type == "normal" or c.type == "dialog") then
        -- buttons for the titlebar
        local buttons = awful.util.table.join(
                awful.button({ }, 1, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.move(c)
                end),
                awful.button({ }, 3, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.resize(c)
                end)
                )

        -- Widgets that are aligned to the left
        local left_layout = wibox.layout.fixed.horizontal()
        left_layout:add(awful.titlebar.widget.iconwidget(c))
        left_layout:buttons(buttons)

        -- Widgets that are aligned to the right
        local right_layout = wibox.layout.fixed.horizontal()
        right_layout:add(awful.titlebar.widget.floatingbutton(c))
        right_layout:add(awful.titlebar.widget.maximizedbutton(c))
        right_layout:add(awful.titlebar.widget.stickybutton(c))
        right_layout:add(awful.titlebar.widget.ontopbutton(c))
        right_layout:add(awful.titlebar.widget.closebutton(c))

        -- The title goes in the middle
        local middle_layout = wibox.layout.flex.horizontal()
        local title = awful.titlebar.widget.titlewidget(c)
        title:set_align("center")
        middle_layout:add(title)
        middle_layout:buttons(buttons)

        -- Now bring it all together
        local layout = wibox.layout.align.horizontal()
        layout:set_left(left_layout)
        layout:set_right(right_layout)
        layout:set_middle(middle_layout)

        awful.titlebar(c):set_widget(layout)
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

-- }}}
