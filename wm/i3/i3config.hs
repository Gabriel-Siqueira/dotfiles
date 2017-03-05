import System.IO

-- Config {{{

i3file           = "/home/gabriel/.config/i3/config"
statusCommand    = "$HOME/bin/conky-i3bar"
startupWorkspace = "default"
-- applications {{{

myMenu             = "\"rofi -matching fuzzy -show run -font 'Michroma 15' -location 1 -columns 5 -lines 1 -width 100 -color-enable -color-window '#222222,#222222,#00ff00' -opacity '100' -separator-style 'solid' -color-normal '#222222, #eeeeee,#222222,#444444,#eeeeee'\""
mySmenu            = "dmenu_run"
myTerminal         =  "terminator"
mySterminal        = "terminology"
myCterminal        = "terminator --profile=Fish"
myScreenShot       = "xfce4-screenshot"

-- }}}
-- colors {{{

focusedWorkspaceBackgroud   = "#0000ee"
activeWorkspaceBackground   = "#0000ee"
inactiveWorkspaceBackground = "#000066"
urgentWorkspaceBackground   = "#ff0000"
barBackground               = "#333333"

-- }}}

-- }}}
-- Main {{{

main = do
        writeFile i3file . unlines $ all
        where
                all = basic ++ variables ++ keys ++ modes ++ colors ++ i3bar ++ fixWorkspaces ++ autoStart ++ start

-- }}}
-- Basic Config {{{

basic =
        [ "font pango:DejaVu Sans Mono 8"
        -- Use Mouse+$mod to drag floating windows to their wanted position
        , "floating_modifier $mod"
        --keyboard layout
        , "setxkbmap br"
        -- no bar on single windom
        , "new_window pixel"
        -- hide border on screen edge
        , "hide_edge_borders smart"
        ]

-- }}}
-- Variables {{{

variables = map (\v -> "set $" ++ fst v ++ " " ++ snd v) variables'
        where
        variables' =
                [ ("mod",       "Mod4")
                ]

-- }}}
-- Keys {{{

addKeys = map (\k -> "bindsym " ++ fst k ++ " " ++ snd k)
keys = addKeys keys'
        where
        keys' = general ++ close ++ focus ++ move ++ workspaces
-- {{{
general =
        -- start a terminal
        -- {{{
        [ ("$mod+Return",        "exec " ++ myTerminal)
        , ("$mod+Shift+Return",  "exec " ++ mySterminal)
        , ("$mod+Control+Return","exec " ++ myCterminal)
        -- }}}
        -- start applications menus
        -- {{{
        , ("$mod+d",      "exec " ++ myMenu)
        , ("$mod+Shift+d","exec " ++ mySmenu)
        -- }}}
        -- resize
        -- {{{
        , ("$mod+r","mode \"resize\"")
        -- }}}
        -- split containers
        -- {{{
        -- split in vertical orientation
        , ("$mod+v","split v")
        , ("$mod+space","mode \"layout\"")
        -- }}}
        -- quit, reload, restart, lock
        -- {{{
        --lock screen
        , ("$mod+KP_Delete",   "exec i3lock -i /home/gabriel/Pictures/dragon.png")
        , ("$mod+KP_Separator","exec i3lock -i /home/gabriel/Pictures/dragon.png")
        -- reload the configuration file
        , ("$mod+e","reload")
        -- restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
        , ("$mod+Shift+r","restart")
        -- exit i3 (logs you out of your X session)
        , ("$mod+Shift+e","exec \"i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -b 'Yes, exit i3' 'i3-msg exit'\"")
        -- }}}
        -- Special keys
        -- {{{
        --Print screen
        , ("Print","exec " ++ myScreenShot)
        --volume
        , ("XF86AudioRaiseVolume", "exec amixer set Master 5%+")
        , ("XF86AudioLowerVolume", "exec amixer set Master 5%-")
        --brightness
        , ("XF86MonBrightnessDown","exec xbacklight -dec 5")
        , ("XF86MonBrightnessUp",  "exec xbacklight -inc 5")
        -- }}}
        -- bar 
        -- {{{
        -- bar toggle, hide or show
        , ("$mod+b","bar mode toggle")
        -- }}}
        ]
-- }}}
-- {{{
close =
        [ ("$mod+BackSpace","kill")
        ]
-- }}}
-- {{{
focus =
        [ ("$mod+h",    "focus left")
        , ("$mod+j",    "focus down")
        , ("$mod+k",    "focus up")
        , ("$mod+l",    "focus right")
        , ("$mod+Left", "focus left")
        , ("$mod+Down", "focus down")
        , ("$mod+Up",   "focus up")
        , ("$mod+Right","focus right")
        , ("$mod+p",    "focus parent")
        , ("$mod+c",    "focus child")
        ]
-- }}}
-- {{{
move =
        [ ("$mod+Shift+h",    "move left")
        , ("$mod+Shift+j",    "move down")
        , ("$mod+Shift+k",    "move up")
        , ("$mod+Shift+l",    "move right")
        , ("$mod+Shift+Left", "move left")
        , ("$mod+Shift+Down", "move down")
        , ("$mod+Shift+Up",   "move up")
        , ("$mod+Shift+Right","move right")
        ]
-- }}}
-- {{{
-- {{{
--  .     dev      mail
-- aux    default  web
-- midia  vm       docs
-- game
-- }}}
workspaces =
        -- switch to workspace
        -- {{{
        [ ("$mod+Mod2+KP_0",    "workspace game")
        , ("$mod+Mod2+KP_1",    "workspace midia")
        , ("$mod+Mod2+KP_2",    "workspace vm")
        , ("$mod+Mod2+KP_3",    "workspace docs")
        , ("$mod+Mod2+KP_4",    "workspace aux")
        , ("$mod+Mod2+KP_5",    "workspace default")
        , ("$mod+Mod2+KP_6",    "workspace web")
        , ("$mod+Mod2+KP_7",    "workspace .")
        , ("$mod+Mod2+KP_8",    "workspace dev")
        , ("$mod+Mod2+KP_9",    "workspace mail")
        , ("$mod+KP_Insert",    "workspace [r]game")
        , ("$mod+KP_End",       "workspace [r]midia")
        , ("$mod+KP_Down",      "workspace [r]vm")
        , ("$mod+KP_Page_Down", "workspace [r]docs")
        , ("$mod+KP_Left",      "workspace [r]aux")
        , ("$mod+KP_Begin",     "workspace [r]default")
        , ("$mod+KP_Right",     "workspace [r]web")
        , ("$mod+KP_Home",      "workspace [r].")
        , ("$mod+KP_Up",        "workspace [r]dev")
        , ("$mod+KP_Page_Up",   "workspace [r]mail")
        , ("$mod+0",            "workspace 0")
        , ("$mod+1",            "workspace 1")
        , ("$mod+2",            "workspace 2")
        , ("$mod+3",            "workspace 3")
        , ("$mod+4",            "workspace 4")
        , ("$mod+5",            "workspace 5")
        , ("$mod+6",            "workspace 6")
        , ("$mod+7",            "workspace 7")
        , ("$mod+8",            "workspace 8")
        , ("$mod+9",            "workspace 9")
        -- }}}
        -- move focused container to workspace
        -- {{{
        , ("$mod+Shift+Mod2+KP_Insert","move container to workspace game")
        , ("$mod+Shift+Mod2+KP_End",  "move container to workspace midia")
        , ("$mod+Shift+Mod2+KP_Down", "move container to workspace vm")
        , ("$mod+Shift+Mod2+KP_Page_Down","move container to workspace docs")
        , ("$mod+Shift+Mod2+KP_Left", "move container to workspace aux")
        , ("$mod+Shift+Mod2+KP_Begin","move container to workspace default")
        , ("$mod+Shift+Mod2+KP_Right","move container to workspace web")
        , ("$mod+Shift+Mod2+KP_Home", "move container to workspace .")
        , ("$mod+Shift+Mod2+KP_Up",   "move container to workspace dev")
        , ("$mod+Shift+Mod2+KP_Page_Up","move container to workspace mail")
        , ("$mod+Shift+KP_Insert",    "move container to workspace [r]game")
        , ("$mod+Shift+KP_End",      "move container to workspace [r]midia")
        , ("$mod+Shift+KP_Down",      "move container to workspace [r]vm")
        , ("$mod+Shift+KP_Page_Down", "move container to workspace [r]docs")
        , ("$mod+Shift+KP_Left",      "move container to workspace [r]aux")
        , ("$mod+Shift+KP_Begin",   "move container to workspace [r]default")
        , ("$mod+Shift+KP_Right",   "move container to workspace [r]web")
        , ("$mod+Shift+KP_Home",    "move container to workspace [r].")
        , ("$mod+Shift+KP_Up",      "move container to workspace [r]dev")
        , ("$mod+Shift+KP_Page_Up", "move container to workspace [r]mail")
        , ("$mod+Shift+1",          "move container to workspace 1")
        , ("$mod+Shift+2",          "move container to workspace 2")
        , ("$mod+Shift+3",          "move container to workspace 3")
        , ("$mod+Shift+4",          "move container to workspace 4")
        , ("$mod+Shift+5",          "move container to workspace 5")
        , ("$mod+Shift+6",          "move container to workspace 6")
        , ("$mod+Shift+7",          "move container to workspace 7")
        , ("$mod+Shift+8",          "move container to workspace 8")
        , ("$mod+Shift+9",          "move container to workspace 9")
        , ("$mod+Shift+0",          "move container to workspace 10")
        -- }}}
        ]
-- }}}

-- }}}
-- Modes {{{

modes = [ "mode \"resize\" {"] ++
        addKeys resize' ++
        addKeys focus ++
        addKeys move ++
        ["}"] ++
        ["mode \"layout\" {"] ++
        addKeys layout' ++
        ["}"]
        where
-- {{{
        resize' =
                [ ("h",     "resize shrink width  10 px or 10 ppt")
                , ("j",     "resize grow   height 10 px or 10 ppt")
                , ("k",     "resize shrink height 10 px or 10 ppt")
                , ("l",     "resize grow   width  10 px or 10 ppt")
                , ("Left",  "resize shrink width  10 px or 10 ppt")
                , ("Down",  "resize grow   height 10 px or 10 ppt")
                , ("Up",    "resize shrink height 10 px or 10 ppt")
                , ("Right", "resize grow   width  10 px or 10 ppt")
                , ("Escape","mode \"default\"")
                ]
-- }}}
        -- {{{
        layout' =
                --move
                [ ("h",       "focus left")
                , ("j",       "focus down")
                , ("k",       "focus up")
                , ("l",       "focus right")
                , ("Left",    "focus left")
                , ("Down",    "focus down")
                , ("Up",      "focus up")
                , ("Right",   "focus right")
                -- enter fullscreen mode for the focused container
                , ("f",       "fullscreen")
                -- change container layout (stacked, tabbed, toggle split)
                , ("s",       "layout stacking")
                , ("t",       "layout tabbed")
                , ("r",       "layout toggle split")
                -- toggle tiling / floating
                , ("Shift+f", "floating toggle")
                -- change focus between tiling / floating windows
                , ("f",       "focus mode_toggle")
                -- focus the parent container
                , ("p",       "focus parent")
                -- focus the child container
                , ("c",       "focus child")
                -- back to normal
                , ("Escape",  "mode \"default\"")
                ]
        -- }}}

-- }}}
-- Colors {{{

colors =
--         class              border  backgr. text    indicator child_border
        [ "client.focused     #222222 #222222 #00ff00 #2e9ef4   #117899"
        , "client.focused_inactive  #333333 #5f676a #ffffff #484e50"
        , "client.unfocused   #333333 #222222 #888888 #292d2e   #333333"
        , "client.urgent      #2f343a #900000 #ffffff #900000"
        , "client.placeholder #000000 #0c0c0c #ffffff #000000"
        , "client.background  #ffffff"
        ]

-- }}}
-- i3bar {{{

i3bar =
        [ "bar {"
        , "    status_command " ++ statusCommand
        , "    position top"
        , "    workspace_buttons yes"
        , "    binding_mode_indicator yes"
        , "    font pango:Terminus 11px"
        , "    colors{"
        , "        background " ++ barBackground
        , "        statusline #eeeeee"
        , "        separator  #666666"
          --       <colorclass>       <border>             <background>                  <text>
        , "        focused_workspace  #00ff00    " ++ focusedWorkspaceBackgroud   ++ "    #ffff00"
        , "        active_workspace   #000000    " ++ activeWorkspaceBackground   ++ "    #ffff00"
        , "        inactive_workspace #000000    " ++ inactiveWorkspaceBackground ++ "    #bbbb00"
        , "        urgent_workspace   #000000    " ++ urgentWorkspaceBackground   ++ "    #ffff00"
        , "        binding_mode       #333333    " ++ barBackground               ++ "    #ff5500"
        , "    }"
        , "}"
        ]

-- }}}
-- Fix workspaces {{{

fixWorkspaces = concat $ zipWith (\w -> map (\c -> "assign [class=\"^" ++ c ++ "$\"] → " ++ w)) workspaces fix
        where
        workspaces = ["mail","dev","web","default","vm","midia","game","docs"]
        fix =
                -- mail
                [ ["thunderbird","TelegramDesktop","Franz"]
                -- dev
                , ["Emacs"]
                -- web
                , ["Chromium","google-chrome","vivaldi-stable"]
                -- default
                , ["Firefox"]
                -- vm
                , ["VirtualBox"]
                -- midia
                , ["Vlc","Kodi","Spotify"]
                -- game
                , ["Steam","Mainwindow.py","Minetest"]
                -- docs
                , ["libreoffice","libreoffice-startcenter","libreoffice-writer","libreoffice-calc","libreoffice-impress","libreoffice-draw","libreoffice-math","libreoffice-base"]
                ]

-- }}}
-- Autostart {{{

autoStart = map ("exec " ++) autoStart'
        where
        autoStart' =
                [ "dropbox"
                , "megasync"
                , "--no-startup-id ~/bin/random_wallpaper.sh"
                , "redshift-gtk"
                , "/opt/franz-bin/Franz"
                , "wicd-client" --tray
                -- , "exec dunst"
                , "firefox"
                ]

-- }}}
-- Startup workspace
start = ["workspace " ++ startupWorkspace ++ " output eDP1"]
-- vim: foldmethod=marker
