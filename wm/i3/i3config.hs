import System.IO
import Control.Monad (when)

-- Config {{{

pc = "GAMa"

i3File           = case pc of
                        "GAMa"    -> "/home/gabriel/.config/i3/config" 
                        "GOLi"    -> "/home/gabriel/.config/i3/config" 
                        "ic"      -> "/home/ec2014/ra155446/.config/i3/config"
                        _         -> "/home/ec2014/ra155446/.i3/config"

conkyFile        = case pc of
                        "GAMa"    -> "/home/gabriel/.conkyrc" 
                        "GOLi"    -> "/home/gabriel/.conkyrc" 
                        "ic"      -> ""
                        _         -> ""

statusCommand    = case pc of
                        "GAMa"    -> "$HOME/bin/conky-i3bar"
                        "GOLi"    -> "$HOME/bin/conky-i3bar"
                        "ic"      -> "i3status"
                        _         -> "i3status"

myWallpaper = case pc of
                        "GAMa"    -> "bubbles.jpg"
                        "GOLi"    -> "flower.jpg"
                        "ic"      -> "wood.jpg"
                        _         -> "clover.jpg"

ws = [
    "game",
    "midi",   "VirM",  "docs",
    "auxE",   "deft",  "auxD",
    "read",   "deve",  "mail",
    "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
startupWorkspace = "deft"

-- applications {{{

myrofi = "\"rofi -matching fuzzy -show run -font 'Michroma 15' -location 1 -columns 5 -lines 1 -width 100 -color-enable -color-window '#222222,#222222,#00ff00' -opacity '100' -separator-style 'solid' -color-normal '#222222, #eeeeee,#222222,#444444,#eeeeee' \""

myMenu             = case pc of
                        "GAMa"    -> myrofi
                        "GOLi"    -> myrofi
                        "ic"      -> "dmenu_run"
                        _         -> "dmenu_run"
mySmenu            = "dmenu_run"
myTerminal         =  case pc of
                        "GAMa"    -> "terminator"
                        "GOLi"    -> "gnome-terminal"
                        "ic"      -> "xfce4-terminal"
                        _         -> "i3-sensible-terminal"
mySterminal        = "terminology"
myCterminal        = "terminator --profile=Fish"
myScreenShot       = "gnome-screenshot"

-- }}}
-- colors {{{

focusedWorkspaceBackgroud   = case pc of
                                "GAMa"    -> "#0000ee"
                                "GOLi"    -> "#6f0582"
                                "ic"      -> "#4B0000#"
                                _         -> "#00aa00"
activeWorkspaceBackground   = case pc of
                                "GAMa"    -> "#0000ee"
                                "GOLi"    -> "#6f0582"
                                "ic"      -> "#4B0000#"
                                _         -> "#00aa00"
inactiveWorkspaceBackground = case pc of
                                "GAMa"    -> "#000066"
                                "GOLi"    -> "#2d0235"
                                "ic"      -> "#290000#"
                                _         -> "#008800"
urgentWorkspaceBackground   = "#ff0000"
barBackground               = "#333333"
focusedWorkspaceBorder      = case pc of
                                "GAMa"    -> "#00ff00"
                                "GOLi"    -> "#fc7b02"
                                "ic"      -> "#00ff00"
                                _         -> "#ffffff"
activeWorkspaceBorder       = case pc of
                                "GAMa"    -> "#000000"
                                "GOLi"    -> "#000000"
                                "ic"      -> "#000000"
                                _         -> "#000000"
inactiveWorkspaceBorder     = case pc of
                                "GAMa"    -> "#000000"
                                "GOLi"    -> "#000000"
                                "ic"      -> "#000000"
                                _         -> ""
urgentWorkspaceBorder      = "#000000"
barBorder                  = "#333333"
-- }}}

-- }}}
-- Main {{{

main = do
        when (i3File /= "")    $ writeFile i3File    . unlines $ all 
        when (conkyFile /= "") $ writeFile conkyFile . unlines $ conky
        where
                all = basic ++ variables ++ keys ++ modes ++ colors ++ i3bar ++ fixWorkspaces ++ autoStart ++ start

-- }}}
-- Basic Config {{{

basic =
        [ "font pango:DejaVu Sans Mono 8"
        -- Use Mouse+$mod to drag floating windows to their wanted position
        , "floating_modifier $mod"
        -- keyboard layout
        , "exec setxkbmap br"
        -- load keyboard changes
        , "exec_always --no-startup-id xmodmap ~/.Xmodmap"
        -- no bar on single windom
        , "new_window pixel"
        -- hide border on screen edge
        , "hide_edge_borders none"
        -- focus does not folow mouse
        , "focus_follows_mouse no"
        -- layout for workspace level
        , "workspace_layout tabbed"
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
        , ("$mod+KP_Delete",        "exec i3lock -t -i ~/Dropbox/Pictures/lock_und_dm/guide_to_the_galaxy.png")
        , ("$mod+Mod2+KP_Separator","exec i3lock -t -i ~/Dropbox/Pictures/lock_und_dm/guide_to_the_galaxy.png")
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
        , ("$mod+F5", "exec amixer set Master 5%+")
        , ("XF86AudioRaiseVolume", "exec amixer set Master 5%+")
        , ("$mod+F6", "exec amixer set Master 5%-")
        , ("XF86AudioLowerVolume", "exec amixer set Master 5%-")
        --brightness
        , ("XF86MonBrightnessDown","exec xbacklight -dec 1")
        , ("$mod+F7","exec xbacklight -dec 1")
        , ("XF86MonBrightnessUp",  "exec xbacklight -inc 1")
        , ("$mod+F8",  "exec xbacklight -inc 1")
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
workspaces =
    -- switch to workspace
    zip ws_key_sw    ( map ("workspace " ++ ) ws ) ++
    zip ws_key_sw_nl ( map ("workspace " ++ ) $ take 10 ws ) ++
    -- move focused container to workspace
    zip ws_key_mv    ( map ("move container to workspace " ++ ) ws ) ++
    zip ws_key_mv_nl ( map ("move container to workspace " ++ ) $ take 10 ws )
    where
        ws_key_sw =    ["$mod+Mod2+KP_"    ++ show x | x <- [0..9]] ++ 
                       ["$mod+"             ++ show x | x <- [0..9]]
        ws_key_sw_nl = ["$mod+KP_"         ++ x      | x <- kp_nl]
        ws_key_mv =    ["$mod+Shift+Mod2+KP_" ++ x      | x <- kp_nl] ++ 
                       ["$mod+Shift+"      ++ show x | x <- [0..9]]
        ws_key_mv_nl = ["$mod+Shift+KP_"      ++ x      | x <- kp_nl]
        kp_nl = ["Insert", "End", "Down", "Page_Down", "Left", "Begin", "Right", "Home", "Up", "Page_Up"]
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
                [ ("h",     "resize shrink width  5 px or 5 ppt")
                , ("j",     "resize grow   height 5 px or 5 ppt")
                , ("k",     "resize shrink height 5 px or 5 ppt")
                , ("l",     "resize grow   width  5 px or 5 ppt")
                , ("Left",  "resize shrink width  5 px or 5 ppt")
                , ("Down",  "resize grow   height 5 px or 5 ppt")
                , ("Up",    "resize shrink height 5 px or 5 ppt")
                , ("Right", "resize grow   width  5 px or 5 ppt")
                , ("Escape","mode \"default\"")
                ]
-- }}}
        -- {{{
        layout' =
                -- focus
                [ ("h",       "focus left")
                , ("j",       "focus down")
                , ("k",       "focus up")
                , ("l",       "focus right")
                , ("Left",    "focus left")
                , ("Down",    "focus down")
                , ("Up",      "focus up")
                , ("Right",   "focus right")
                -- move
                , ("Shift+h",    "move left")
                , ("Shift+j",    "move down")
                , ("Shift+k",    "move up")
                , ("Shift+l",    "move right")
                , ("Shift+Left", "move left")
                , ("Shift+Down", "move down")
                , ("Shift+Up",   "move up")
                , ("Shift+Right","move right")
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
          --       <colorclass>            <border>                       <background>                        <text>
        , "        focused_workspace  " ++ focusedWorkspaceBorder  ++ " " ++ focusedWorkspaceBackgroud   ++ "    #ffff00"
        , "        active_workspace   " ++ activeWorkspaceBorder   ++ " " ++ activeWorkspaceBackground   ++ "    #ffff00"
        , "        inactive_workspace " ++ inactiveWorkspaceBorder ++ " " ++ inactiveWorkspaceBackground ++ "    #bbbb00"
        , "        urgent_workspace   " ++ urgentWorkspaceBorder   ++ " " ++ urgentWorkspaceBackground   ++ "    #ffff00"
        , "        binding_mode       " ++ barBorder               ++ " " ++ barBackground               ++ "    #ff5500"
        , "    }"
        , "}"
        ]

-- }}}
-- Fix workspaces {{{

fixWorkspaces = concat $ zipWith (\w -> map (\c -> "assign [class=\"^" ++ c ++ "$\"] → " ++ w)) ws fix ++ 
                zipWith (\w -> map (\c -> "for_window [class=\"^" ++ c ++ "$\"] move to workspace " ++ w)) workspacesWin fixWin
        where
        workspaces = ["mail","dev","auxE","auxD","default","vm","midia","game","docs"]
        fix =
                -- game
                [ ["Steam","Mainwindow.py","Minetest"]
                -- midia
                , ["Vlc"]
                -- vm
                , ["VirtualBox"]
                -- docs
                , ["libreoffice","libreoffice-startcenter","libreoffice-writer","libreoffice-calc","libreoffice-impress","libreoffice-draw","libreoffice-math","libreoffice-base"]
                -- auxE
                , ["Terminator","Gnome-terminal","Xfce4-terminal"]
                -- default
                , ["Firefox"]
                -- auxD
                , ["Chromium","google-chrome","vivaldi-stable"]
                -- read
                , ["calibre"]
                -- dev
                , ["Emacs"]
                -- mail
                , ["thunderbird","TelegramDesktop","Franz"]
                ]
        workspacesWin = [ws !! 1]
        fixWin =
                -- midia
                [ ["Kodi","Spotify"]
                ]

-- }}}
-- Autostart {{{

autoStart = map (\x -> if x == "" then "" else "exec " ++ x) autoStart'
        where
        autoStart' =
                [ case pc of
                        "GAMa"    -> "dropbox" 
                        "GOLi"    -> "dropbox" 
                        "ic"      -> "~/.dropbox-dist/dropboxd"
                        _         -> ""
                , myTerminal
                , "megasync"
                , ""
                , "feh --bg-fill ~/Dropbox/Pictures/mywallpaper/" ++ myWallpaper
                , "redshift-gtk"
                , case pc of
                        "GAMa"    -> "franz"
                        "GOLi"    -> "/home/gabriel/application/franz/Franz"
                        "ic"      -> ""
                        _         -> ""
                , "nm-applet"
                -- , "dunst"
                , "twmnd"
                , "~/bin/bat.sh"
                , case pc of
                        "GAMa"    -> "firefox"
                        "GOLi"    -> "firefox"
                        "ic"      -> "firefox"
                        _         -> "firefox"
                ]

-- }}}
-- Conky {{{

conky = base ++ text 
-- Settings {{{

updateComand = case pc of
                        "GAMa"    -> "checkupdates | wc -l" 
                        "GOLi"    -> "/usr/lib/update-notifier/apt-check --human-readable  | sed -n '1,1p' | cut -d ' ' -f 1"
                        "ic"      -> ""
                        _         -> ""
wifi = case pc of
                        "GAMa"    -> "wlp2s0" 
                        "GOLi"    -> "wlp1s0"
                        "ic"      -> ""
                        _         -> ""
eth = case pc of
                        "GAMa"    -> "p2p1" 
                        "GOLi"    -> "enp2s0f5"
                        "ic"      -> ""
                        _         -> ""
bat = case pc of
                        "GAMa"    -> "BAT1" 
                        "GOLi"    -> "BAT0"
                        "ic"      -> ""
                        _         -> ""

-- }}}
-- base {{{

base =
        [ "background no"
        , "out_to_console yes"
        , "out_to_x no"
        , "max_text_width 0"
        , "own_window no"
        , "update_interval 0.5"
        , "total_run_times 0"
        , "short_units yes"
        , "if_up_strictness address"
        , "use_spacer right"
        , "override_utf8_locale no"
        , "cpu_avg_samples 2"
        , "TEXT"
        ]

-- }}}
-- text {{{

text = ["["] ++ toJason text' ++ ["],"]
toJason [t]    = ["{" ++ toJason' t ++ "}"]
toJason (t:ts) = ("{" ++ toJason' t ++ "},\\") : toJason ts
toJason' []         = ""
toJason' [(h,t)]    = h ++ ":" ++ t
toJason' ((h,t):xs) = h ++ ":" ++ t ++ "," ++ toJason' xs

-- }}}

text' = [
-- Disk Space 
                -- {{{
                [ ("\"full_text\""  ,"\" ◙\"")
                , ("\"color\""      ,"\"\\#B538AB\"")
                , ("\"separator\""  ,"false")
                , ("\"separator_block_width\"","6")
                ],
                [ ("\"full_text\""  ,"\"[$fs_used/$fs_size] \"")
                , ("\"color\""      ,"\"\\#AAAA00\"")
                , ("\"separator\""  ,"true")
                , ("\"separator_block_width\"","6")
                ],
                -- }}}
-- Memory
                -- {{{
                [ ("\"full_text\""  ,"\" ⚅\"")
                , ("\"color\""      ,"\"\\#B538AB\"")
                , ("\"separator\""  ,"false")
                , ("\"separator_block_width\"","6")
                ],
                [ ("\"full_text\""  ,"\"[$mem/$memmax] \"")
                , ("\"color\""      ,"\"\\#AAAA00\"")
                , ("\"separator\""  ,"true")
                , ("\"separator_block_width\"","6")
                ],
                -- }}}
-- CPU
                -- {{{
                [ ("\"full_text\""  ,"\" ⚛\"")
                , ("\"color\""      ,"\"\\#FFFFFF\"")
                , ("\"separator\""  ,"false")
                , ("\"separator_block_width\"","6")
                ],
                [ ("\"full_text\""  ,"\"[${cpu cpu1}%,\"")
                , ("\"color\""      ,"\"\\#AAAA00\"")
                , ("\"separator\""  ,"false")
                , ("\"separator_block_width\"","6")
                ],
                [ ("\"full_text\""  ,"\"${cpu cpu2}%,\"")
                , ("\"color\""      ,"\"\\#AAAA00\"")
                , ("\"separator\""  ,"false")
                , ("\"separator_block_width\"","6")
                ],
                [ ("\"full_text\""  ,"\"${cpu cpu3}%,\"")
                , ("\"color\""      ,"\"\\#AAAA00\"")
                , ("\"separator\""  ,"false")
                , ("\"separator_block_width\"","6")
                ],
                [ ("\"full_text\""  ,"\"${cpu cpu4}%] \"")
                , ("\"color\""      ,"\"\\#AAAA00\"")
                , ("\"separator\""  ,"true")
                , ("\"separator_block_width\"","6")
                ],
                -- }}}
-- Package manager (Pacman, apt, ...)
                -- {{{
                [ ("\"full_text\""  ,"\" ⇑\"")
                , ("\"color\""      ,"\"\\#FF6200\"")
                , ("\"separator\""  ,"false")
                , ("\"separator_block_width\"","6")
                ],
                [ ("\"full_text\""  ,"\"[${exec " ++ updateComand ++ "}] \"")
                , ("\"color\""      ,"\"\\#AAAA00\"")
                , ("\"separator\""  ,"true")
                , ("\"separator_block_width\"","6")
                ],
                -- }}}
-- Wifi
                -- {{{
                [ ("\"full_text\""  ,"\" ☎\"")
                , ("\"color\""      ,"${if_existing /proc/net/route " ++ wifi ++ "}\"\\#00FF00\"$else\"\\#FF0000\" ${endif}")
                , ("\"separator\""  ,"false"),("\"separator_block_width\"","6")
                ],
                [ ("\"full_text\""  ,"\"[${wireless_link_qual_perc " ++ wifi ++ "}% - ${wireless_bitrate " ++ wifi ++ "}]\"")
                , ("\"color\""      ,"\"\\#AAAA00\"")
                , ("\"separator\""  ,"false")
                , ("\"separator_block_width\"","6")
                ],
                -- }}}
-- Net
                -- {{{
                [ ("\"full_text\""  ,"${if_existing /proc/net/route " ++ eth ++ "}\"- [${wireless_bitrate " ++ eth ++ "}] \"$else\" \"${endif}")
                , ("\"color\""      ,"\"\\#AAAA00\"")
                , ("\"separator\""  ,"true")
                , ("\"separator_block_width\"","6")
                ],
                -- }}}
-- Volume
                -- {{{
                [ ("\"full_text\""  ,"\" ♫\"")
                , ("\"color\""      ,"\"\\#FFFF00\"")
                , ("\"separator\""  ,"false")
                , ("\"separator_block_width\"","6")
                ],
                [ ("\"full_text\""  ,"\"${exec amixer -c 0 get Master | grep Mono: | cut -d \" \" -f6} \"")
                , ("\"color\""      ,"\"\\#AAAA00\"")
                , ("\"separator\""  ,"true")
                , ("\"separator_block_width\"","6")],
                -- }}}
-- Brighness
                -- {{{
                [ ("\"full_text\""  ,"\" ☀\"")
                , ("\"color\""      ,"\"\\#FFFF00\"")
                , ("\"separator\""  ,"false")
                , ("\"separator_block_width\"","6")
                ],
                [ ("\"full_text\""  ,"\"[${exec xbacklight | cut -d \".\" -f 1}%] \"")
                , ("\"color\""      ,"\"\\#AAAA00\"")
                , ("\"separator\""  ,"true")
                , ("\"separator_block_width\"","6")
                ],
                -- }}}
-- Battery
                -- {{{
                [ ("\"full_text\"","\" ⚡\"")
                , ("\"color\"","${if_match ${battery_percent " ++ bat ++ "} >= 20 }\"\\#00FF00\"$else\"\\#FF0000\" ${endif}") 
                , ("\"separator\"","false")
                , ("\"separator_block_width\"","6")
                ],
                [ ("\"full_text\"","\"[${battery_percent " ++ bat ++ "}%] \"")
                , ("\"color\"","\"\\#AAAA00\"")
                , ("\"separator\"","true")
                , ("\"separator_block_width\"","6")
                ],
                -- }}}
-- Calender
                -- {{{
                [ ("\"full_text\"","\" <\"")
                , ("\"color\"","\"\\#2E9AFE\"")
                , ("\"separator\"","false")
                , ("\"separator_block_width\"","6")
                ],
                [ ("\"full_text\"","\"${time %a %b %d}\"")
                , ("\"color\"","\"\\#AAAA00\"")
                , ("\"separator\"","true")
                ],
                -- }}}
-- Time
                -- {{{
                [ ("\"full_text\"","\"${time %H:%M:%S}\"")
                , ("\"color\"","\"\\#AAAA00\"")
                , ("\"separator\"","false")
                ],
                [ ("\"full_text\"","\">\"")
                , ("\"color\"","\"\\#2E9AFE\"")
                , ("\"separator\"","false")
                , ("\"separator_block_width\"","6")
                ]
                -- }}}
        ]

-- }}}
-- Startup workspace {{{

start = ["exec --no-startup-id i3-msg workspace " ++ startupWorkspace]

-- }}}
-- vim: foldmethod=marker foldlevel=0
