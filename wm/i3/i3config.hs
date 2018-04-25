import System.IO
import Control.Monad (when)

-- Config {{{

-- basic {{{

pc = "GAMa"
-- {{{
i3File           = case pc of
                        "GAMa"    -> "/home/gabriel/.config/i3/config" 
                        "GOLi"    -> "/home/gabriel/.config/i3/config" 
                        "ic"      -> "/home/ec2014/ra155446/.config/i3/config"
                        _         -> "/home/ec2014/ra155446/.config/i3/config"
-- }}}
-- {{{
conkyFile        = case pc of
                        "GAMa"    -> "/home/gabriel/.conkyrc" 
                        "GOLi"    -> "/home/gabriel/.conkyrc" 
                        "ic"      -> ""
                        _         -> ""
-- }}}
-- {{{
polyFile        = case pc of
                        "GAMa"    -> "/home/gabriel/.config/polybar/config" 
                        "GOLi"    -> "/home/gabriel/.config/polybar/config" 
                        "ic"      -> ""
                        _         -> ""
-- }}}
---{{{
statusCommand    = case pc of
                        "GAMa"    -> "$HOME/bin/conky-i3bar"
                        "GOLi"    -> "$HOME/bin/conky-i3bar"
                        "ic"      -> "i3status"
                        _         -> "i3status"
--}}}
--{{{
myWallpaper      = case pc of
                        "GAMa"    -> "bubbles.jpg"
                        "GOLi"    -> "flower.jpg"
                        "ic"      -> "wood.jpg"
                        _         -> "clover.jpg"
--}}}

startupWorkspace = "deft"
--{{{
ws = [
    "game",
    "midi",   "VirM",  "docs",
    "auxE",   "deft",  "auxD",
    "read",   "deve",  "mail",
    "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
--}}}

-- }}}
-- applications {{{

-- {{{
myMainTerminal     =  case pc of
                        "GAMa"    -> "termite -e tmux --name=tmux"
                        "GOLi"    -> "gnome-terminal"
                        "ic"      -> "xfce4-terminal"
                        _         -> "i3-sensible-terminal"
-- }}}
-- {{{
myTerminal         =  case pc of
                        "GAMa"    -> "termite"
                        "GOLi"    -> "termite"
                        "ic"      -> "urxvt"
                        _         -> "i3-sensible-terminal"
-- }}}
-- {{{
myrofi             = "\"rofi -matching fuzzy -show run -font 'Michroma 15' -location 1 -columns 5 -lines 1 -width 100 -color-enable -color-window '#222222,#222222,#00ff00' -opacity '100' -separator-style 'solid' -color-normal '#222222, #eeeeee,#222222,#444444,#eeeeee' \""
--}}}
--{{{
myMenu             = case pc of
                        "GAMa"     -> myrofi
                        "GOLi"     -> myrofi
                        "ic"       -> "dmenu_run"
                        _          -> "dmenu_run"
--}}}
mySTerminal = myMainTerminal
mySmenu            = "dmenu_run"
myScreenShot       = "gnome-screenshot -a"

-- }}}
-- colors {{{

barBackground               = "#111111"
barBorder                   = "#111111"
--{{{
focusedWorkspaceBackgroud   = case pc of
                                "GAMa"    -> "#111111"
                                "GOLi"    -> "#111111"
                                "ic"      -> "#111111"
                                _         -> "#111111"
--}}}
--{{{
focusedWorkspaceBorder      = case pc of
                                "GAMa"    -> "#111111"
                                "GOLi"    -> "#111111"
                                "ic"      -> "#111111"
                                _         -> "#111111"
--}}}
--{{{
activeWorkspaceBackground   = case pc of
                                "GAMa"    -> "#111111"
                                "GOLi"    -> "#111111"
                                "ic"      -> "#111111#"
                                _         -> "#111111"
--}}}
--{{{
activeWorkspaceBorder       = case pc of
                                "GAMa"    -> "#111111"
                                "GOLi"    -> "#111111"
                                "ic"      -> "#111111"
                                _         -> "#111111"
--}}}
--{{{
inactiveWorkspaceBackground = case pc of
                                "GAMa"    -> "#111111"
                                "GOLi"    -> "#111111"
                                "ic"      -> "#111111#"
                                _         -> "#111111"
--}}}
--{{{
inactiveWorkspaceBorder     = case pc of
                                "GAMa"    -> "#111111"
                                "GOLi"    -> "#111111"
                                "ic"      -> "#111111"
                                _         -> ""
--}}}
--{{{
urgentWorkspaceBackground   = case pc of
                                "GAMa"    -> "#111111"
                                "GOLi"    -> "#111111"
                                "ic"      -> "#111111"
                                _         -> "#111111"
--}}}
--{{{
urgentWorkspaceBorder       = case pc of
                                "GAMa"    -> "#111111"
                                "GOLi"    -> "#111111"
                                "ic"      -> "#111111"
                                _         -> ""
--}}}

-- }}}

-- }}}
-- Main {{{

main = do
        when (i3File /= "")    $ writeFile i3File    . unlines $ all 
        when (conkyFile /= "") $ writeFile conkyFile . unlines $ conky
        -- when (polyFile /= "")  $ writeFile polyFile . unlines $ poly
        where
                all = basic ++ keys ++ modes ++ colors ++ i3bar ++ fixPlace ++ autoStart ++ start

-- }}}
-- Basic Config {{{

basic =
        [ "font pango:DejaVu Sans Mono 0"
        -- Use Mouse+$mod to drag floating windows to their wanted position
        , "floating_modifier $mod"
        -- keyboard layout
        , "exec setxkbmap br"
        -- no bar on single windom
        , "new_window pixel"
        -- hide border on screen edge
        , "hide_edge_borders none"
        -- focus does not folow mouse
        , "focus_follows_mouse no"
        -- layout for workspace level
        , "workspace_layout tabbed"
        -- mode key
        , "set $mod Mod4"
        ]

-- }}}
-- Keys {{{

addKeys = map (\k -> "bindsym " ++ fst k ++ " " ++ snd k)
keys = addKeys keys'
        where
        keys' = general ++ close ++ focus ++ move ++ workspaces ++ scratchpad
-- {{{
general =
        -- start a terminal
        -- {{{
        [ ("$mod+Return",        "exec " ++ myTerminal)
        , ("$mod+Shift+Return","exec " ++ mySTerminal)
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
-- {{{
scratchpad =
        [ ("$mod+m", "[instance=\"math\"] scratchpad show; [instance=\"math\"] move position center; mode \"sp-math\"")
        , ("$mod+n", "[instance=\"note\"] scratchpad show; [instance=\"note\"] move position center; mode \"sp-note\"")
        , ("$mod+f", "[instance=\"file\"] scratchpad show; [instance=\"file\"] move position center; mode \"sp-file\"")
        ]
-- }}}

-- }}}
-- Modes {{{

modes = [ "mode \"resize\" {"] ++ addKeys resize'
        ++ addKeys focus ++ addKeys move ++ ["}"] ++
        ["mode \"layout\" {"]  ++ addKeys layout'
        ++ ["}"] ++
        ["mode \"sp-math\" {"] ++ addKeys spMath
        ++ addKeys scratchpad' ++["}"] ++
        ["mode \"sp-note\" {"]  ++ addKeys spNote
        ++ addKeys scratchpad' ++ ["}"] ++
        ["mode \"sp-file\" {"] ++ addKeys spFile
        ++ addKeys scratchpad' ++ ["}"]
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
        -- {{{
        spMath = 
            [("$mod+m", "[instance=\"math\"] focus; [instance=\"math\"] scratchpad show; mode \"default\"")
            , ("$mod+s", "[instance=\"math\"] focus") 
            ]
        -- }}}
        -- {{{
        spNote = 
            [("$mod+n", "[instance=\"note\"] focus; [instance=\"note\"] scratchpad show; mode \"default\"")
            , ("$mod+s", "[instance=\"note\"] focus") 
            ]
        -- }}}
        -- {{{
        spFile = 
            [("$mod+f", "[instance=\"file\"] focus; [instance=\"file\"] scratchpad show; mode \"default\"")
            , ("$mod+s", "[instance=\"file\"] focus") 
            ]
        -- }}}
        -- {{{
        scratchpad' =
            -- move
            [ ("$mod+h",        "move left 20px")
            , ("$mod+j",        "move down 20px")
            , ("$mod+k",        "move up 20px")
            , ("$mod+l",        "move right 20px")
            , ("$mod+Left",     "move left 20px")
            , ("$mod+Down",     "move down 20px")
            , ("$mod+Up",       "move up 20px")
            , ("$mod+Right",    "move right 20px")
            , ("$mod+space",    "move position 0px 0px")
            , ("$mod+Escape",   "mode \"default\"")
            ]
        -- }}}

-- }}}
-- Colors {{{

colors =
--         class              border  backgr. text    indicator child_border
        [ "client.focused     #aaaaaa #aaaaaa #00ff00 #111111 #111111"
        , "client.unfocused   #aaaaaa #111111 #888888 #111111 #111111"
        , "client.urgent      #ff0000 #ff0000 #ff0000 #111111 #111111"
        , "client.placeholder #111111 #111111 #ffffff #111111 #111111"
        , "client.focused_inactive  #aaaaaa #111111 #ffffff #111111 #111111"
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
        , "        urgent_workspace   " ++ urgentWorkspaceBorder   ++ " " ++ urgentWorkspaceBackground   ++ "    #ff0000"
        , "        binding_mode       " ++ barBorder               ++ " " ++ barBackground               ++ "    #ff5500"
        , "    }"
        , "}"
        ]

-- }}}
-- Fix Place {{{

fixPlace = concat $ fixWp ++ scratchpad
        where
        fixWp = map (\(sel, wp, cs) -> map (\c -> "for_window " ++ sel ++ c ++ "$\"] move to workspace " ++ wp) cs) fix
        scratchpad = map (\(ins, coms) -> map (\com -> "for_window " ++ win ++ ins ++ "$\"] " ++ com) coms) sp

        ass = "[class=\"^"
        wcl = "[class=\"^"
        win = "[instance=\"^"
-- {{{
        fix =
            [ (ass, "game",  ["Steam","Mainwindow.py","Minetest"])
            , (ass, "midi", ["Vlc"])
            , (wcl, "midi", ["Kodi","Spotify"])
            , (ass, "virM", ["VirtualBox"])
            , (ass, "docs", ["libreoffice","libreoffice-startcenter","libreoffice-writer","libreoffice-calc","libreoffice-impress","libreoffice-draw","libreoffice-math","libreoffice-base"])
            , (ass, "auxE", ["Gnome-terminal","Xfce4-terminal"])
            , (win, "auxE", ["tmux"])
            , (ass, "deft", ["Firefox"])
            , (ass, "auxD", ["Chromium","google-chrome","vivaldi-stable", "Opera"])
            , (ass, "read", ["calibre"])
            , (ass, "deve", ["Emacs"])
            , (ass, "mail", ["thunderbird","TelegramDesktop","Franz"])
            ]
-- }}}
-- {{{
        sp = [ ("file", ["move scratchpad","floating enable","resize set 1100 600"])
             , ("note", ["move scratchpad","floating enable","resize set 1200 700"])
             , ("math", ["move scratchpad","floating enable","resize set 800 300"])
             ]
-- }}}


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
                , case pc of
                        "GAMa"    -> "franz"
                        "GOLi"    -> "/home/gabriel/application/franz/Franz"
                        "ic"      -> ""
                        _         -> ""
                , myMainTerminal
                , "compton"
                , "megasync"
                , "feh --bg-fill ~/Dropbox/Pictures/mywallpaper/" ++ myWallpaper
                , "redshift-gtk"
                , "nm-applet"
                -- , "dunst"
                , "twmnd"
                , "~/bin/bat.sh"
                , "firefox"
                , term_lauch ++ "math -e ghci"
                , term_lauch ++ "file -e ranger"
                , term_lauch ++ "note -e sncli"
                ]
        term_lauch = case pc of
                    "GAMa"    -> "termite --name="
                    "GOLi"    -> "termite --name="
                    "ic"      -> "urxvt -name "
                    _         -> "urxvt -name "

-- }}}
-- Startup workspace {{{

start = ["exec --no-startup-id i3-msg workspace " ++ startupWorkspace]

-- }}}
-- Bars {{{

-- Settings {{{

conky_text_color = "#BBBBBB"

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
-- Conky {{{

conky = base ++ text 
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

text' = [
-- Disk Space 
                -- {{{
                [ ("\"full_text\""  ,"\" ◙\"")
                , ("\"color\""      ,"\"\\#B538AB\"")
                , ("\"separator\""  ,"false")
                , ("\"separator_block_width\"","6")
                ],
                [ ("\"full_text\""  ,"\"[$fs_used/$fs_size] \"")
                , ("\"color\""      ,"\"\\" ++ conky_text_color ++ "\"")
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
                , ("\"color\""      ,"\"\\" ++ conky_text_color ++ "\"")
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
                , ("\"color\""      ,"\"\\" ++ conky_text_color ++ "\"")
                , ("\"separator\""  ,"false")
                , ("\"separator_block_width\"","6")
                ],
                [ ("\"full_text\""  ,"\"${cpu cpu2}%,\"")
                , ("\"color\""      ,"\"\\" ++ conky_text_color ++ "\"")
                , ("\"separator\""  ,"false")
                , ("\"separator_block_width\"","6")
                ],
                [ ("\"full_text\""  ,"\"${cpu cpu3}%,\"")
                , ("\"color\""      ,"\"\\" ++ conky_text_color ++ "\"")
                , ("\"separator\""  ,"false")
                , ("\"separator_block_width\"","6")
                ],
                [ ("\"full_text\""  ,"\"${cpu cpu4}%] \"")
                , ("\"color\""      ,"\"\\" ++ conky_text_color ++ "\"")
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
                , ("\"color\""      ,"\"\\" ++ conky_text_color ++ "\"")
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
                , ("\"color\""      ,"\"\\" ++ conky_text_color ++ "\"")
                , ("\"separator\""  ,"false")
                , ("\"separator_block_width\"","6")
                ],
                -- }}}
-- Net
                -- {{{
                [ ("\"full_text\""  ,"${if_existing /proc/net/route " ++ eth ++ "}\"- [${wireless_bitrate " ++ eth ++ "}] \"$else\" \"${endif}")
                , ("\"color\""      ,"\"\\" ++ conky_text_color ++ "\"")
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
                , ("\"color\""      ,"\"\\" ++ conky_text_color ++ "\"")
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
                , ("\"color\""      ,"\"\\" ++ conky_text_color ++ "\"")
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
                , ("\"color\"","\"\\" ++ conky_text_color ++ "\"")
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
                , ("\"color\"","\"\\" ++ conky_text_color ++ "\"")
                , ("\"separator\"","true")
                ],
                -- }}}
-- Time
                -- {{{
                [ ("\"full_text\"","\"${time %H:%M:%S}\"")
                , ("\"color\"","\"\\" ++ conky_text_color ++ "\"")
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

-- }}}
-- Polybar {{{

poly = colors ++ bar ++ xwindow ++ xkeyboard ++ filesystem ++ bspwm ++ i3 ++ mpd ++ xbacklight ++ xbacklight_acpi ++ cpu ++ memory ++ wlan ++ eth ++ date ++ volume ++ battery ++ powermenu ++ settings ++ global_wm
    where
    mod_left = "i3"
    mod_cent = ""
    mod_right = "filesystem cpu memory xkeyboard wlan eth volume xbacklight battery temperature date powermenu"
    -- {{{
    colors =
        [ "[colors]"
        , "background = #111"
        , "background-alt = #444"
        , "foreground = #bbbbbb"
        , "foreground-alt = #555"
        , "primary = #ffb52a"
        , "secondary = #e60053"
        , "alert = #bd2c40"
        ]
        --}}}
        ---{{{
    bar =
        [ "[bar/main]"
        , "width = 100%"
        , "height = 15"
        , "radius = 0.0"
        , "fixed-center = false"
        , "background = ${colors.background}"
        , "foreground = ${colors.foreground}"
        , "line-size = 3"
        , "line-color = #f00"
        , "border-size = 0"
        , "border-color = #00000000"
        , "padding-left = 0"
        , "padding-right = 2"
        , "module-margin-left = 1"
        , "module-margin-right = 2"
        , "font-0 = fixed:pixelsize=10;1"
        , "font-1 = unifont:fontformat=truetype:size=8:antialias=false;0"
        , "font-2 = siji:pixelsize=10;1"
        , "modules-left = " ++ mod_left
        , "modules-center = " ++ mod_cent
        , "modules-right = " ++ mod_right
        , "tray-position = right"
        , "tray-padding = 2"
        , "tray-detached = false"
        , "cursor-click = pointer"
        , "cursor-scroll = ns-resize"
        ]
-- }}}
-- {{{
    xwindow =
        ["[module/xwindow]"
        , "type = internal/xwindow"
        , "label = %title:0:30:...%"
        ]
-- }}}
-- {{{
    xkeyboard =
        [ "[module/xkeyboard]"
        , "type = internal/xkeyboard"
        , "blacklist-0 = num lock"

        , "format-prefix-foreground = ${colors.foreground-alt}"
        , "format-prefix-underline = ${colors.secondary}"

        , "label-layout = %layout%"
        , "label-layout-underline = ${colors.secondary}"

        , "label-indicator-padding = 2"
        , "label-indicator-margin = 1"
        , "label-indicator-background = ${colors.secondary}"
        , "label-indicator-underline = ${colors.secondary}"
        ]
        -- }}}
        -- {{{
    filesystem =
        [ "[module/filesystem]"
        , "type = internal/fs"
        , "interval = 25"

        , "mount-0 = /"

        , "label-mounted = %{F#0a81f5}%mountpoint%%{F-} %free%/%total%"
        , "label-unmounted = %mountpoint% not mounted"
        , "label-unmounted-foreground = ${colors.foreground-alt}"
        ]
        -- }}}
    -- {{{
    bspwm =
        [ "[module/bspwm]"
        , "type = internal/bspwm"
        , "label-focused = %index%"
        , "label-focused-background = ${colors.background-alt}"
        , "label-focused-underline= ${colors.primary}"
        , "label-focused-padding = 2"
        , "label-occupied = %index%"
        , "label-occupied-padding = 2"
        , "label-urgent = %index%!"
        , "label-urgent-background = ${colors.alert}"
        , "label-urgent-padding = 2"
        , "label-empty = %index%"
        , "label-empty-foreground = ${colors.foreground-alt}"
        , "label-empty-padding = 2"
        ]
    -- }}}
    -- {{{
    i3 =
        [ "[module/i3]"
        , "type = internal/i3"
        , "format = <label-state> <label-mode>"
        , "index-sort = true"
        , "wrapping-scroll = false"
        -- Only show workspaces on the same output as the bar"
        -- , "pin-workspaces = true"
        , "label-mode-padding = 2"
        , "label-mode-foreground = #000"
        , "label-mode-background = ${colors.primary}"
        , "label-focused = %name%"
        , "label-focused-background = ${module/bspwm.label-focused-background}"
        , "label-focused-underline = ${module/bspwm.label-focused-underline}"
        , "label-focused-padding = ${module/bspwm.label-focused-padding}"
        , "label-unfocused = %name%"
        , "label-unfocused-padding = ${module/bspwm.label-occupied-padding}"
        , "label-visible = %name%"
        , "label-visible-background = ${self.label-focused-background}"
        , "label-visible-underline = ${self.label-focused-underline}"
        , "label-visible-padding = ${self.label-focused-padding}"
        , "label-urgent = %name%"
        , "label-urgent-background = ${module/bspwm.label-urgent-background}"
        , "label-urgent-padding = ${module/bspwm.label-urgent-padding}"
        ]
    -- }}}
    -- {{{
    mpd =
        [ "[module/mpd]"
        , "type = internal/mpd"
        , "format-online = <label-song>  <icon-prev> <icon-stop> <toggle> <icon-next>"

        , "icon-prev = "
        , "icon-stop = "
        , "icon-play = "
        , "icon-pause = "
        , "icon-next = "

        , "label-song-maxlen = 25"
        , "label-song-ellipsis = true"
        ]
    -- }}}
    -- {{{
    xbacklight =
        [ "[module/xbacklight]"
        , "type = internal/xbacklight"

        , "format = <label>"
        , "label = %percentage%%"
        , "format-underline = #ffff00"

        , "bar-width = 10"
        , "bar-indicator = |"
        , "bar-indicator-foreground = #ff"
        , "bar-indicator-font = 2"
        , "bar-fill = ─"
        , "bar-fill-font = 2"
        , "bar-fill-foreground = #9f78e1"
        , "bar-empty = ─"
        , "bar-empty-font = 2"
        , "bar-empty-foreground = ${colors.foreground-alt}"
        ]
    -- }}}
    -- {{{
    xbacklight_acpi =
        [ "[module/backlight-acpi]"
        , "inherit = module/xbacklight"
        , "type = internal/backlight"
        , "card = intel_backlight"
        ]
    -- }}}
    -- {{{
    cpu =
        [ "[module/cpu]"
        , "type = internal/cpu"
        , "interval = 2"
        , "format-prefix = "
        , "format-prefix-foreground = ${colors.foreground-alt}"
        , "format-underline = #f90000"
        , "label = %percentage:2%%"
        ]
    -- }}}
    -- {{{
    memory =
        [ "[module/memory]"
        , "type = internal/memory"
        , "interval = 2"
        , "format-prefix = "
        , "format-prefix-foreground = ${colors.foreground-alt}"
        , "format-underline = #4bffdc"
        , "label = %gb_free%/%gb_total%"
        ]
    -- }}}
    -- {{{
    wlan =
        [ "[module/wlan]"
        , "type = internal/network"
        , "interface = " ++ wifi
        , "interval = 3.0"

        , "format-connected = <ramp-signal> <label-connected>"
        , "format-connected-underline = #9f78e1"
        , "label-connected = %essid%"

        , "format-disconnected ="
        , ";format-disconnected = <label-disconnected>"
        , ";format-disconnected-underline = ${self.format-connected-underline}"
        , ";label-disconnected = %ifname% disconnected"
        , ";label-disconnected-foreground = ${colors.foreground-alt}"

        , "ramp-signal-0 = "
        , "ramp-signal-1 = "
        , "ramp-signal-2 = "
        , "ramp-signal-3 = "
        , "ramp-signal-4 = "
        , "ramp-signal-foreground = ${colors.foreground-alt}"
        ]
    -- }}}
    -- {{{
    eth =
        [ "[module/eth]"
        , "type = internal/network"
        , "interface = enp1s0f0"
        , "interval = 3.0"

        , "format-connected-underline = #55aa55"
        , "format-connected-prefix = "
        , "format-connected-prefix-foreground = ${colors.foreground-alt}"
        , "label-connected = %local_ip%"

        , "format-disconnected ="
        , ";format-disconnected = <label-disconnected>"
        , ";format-disconnected-underline = ${self.format-connected-underline}"
        , ";label-disconnected = %ifname% disconnected"
        , ";label-disconnected-foreground = ${colors.foreground-alt}"
        ]
    -- }}}
    -- {{{
    date =
        [ "[module/date]"
        , "type = internal/date"
        , "interval = 5"

        , "date ="
        , "date-alt = \" %Y-%m-%d\""

        , "time = %H:%M"
        , "time-alt = %H:%M:%S"

        , "format-prefix = "
        , "format-prefix-foreground = ${colors.foreground-alt}"
        , "format-underline = #0a6cf5"

        , "label = %date% %time%"
        ]
    -- }}}
    -- {{{
    volume =
        [ "[module/volume]"
        , "type = internal/volume"

        , "format-volume = <label-volume> <bar-volume>"
        , "label-volume = VOL"
        , "label-volume-foreground = ${root.foreground}"

        , "format-muted-prefix = "
        , "format-muted-foreground = ${colors.foreground-alt}"
        , "label-muted = sound muted"

        , "bar-volume-width = 10"
        , "bar-volume-foreground-0 = #55aa55"
        , "bar-volume-foreground-1 = #55aa55"
        , "bar-volume-foreground-2 = #55aa55"
        , "bar-volume-foreground-3 = #55aa55"
        , "bar-volume-foreground-4 = #55aa55"
        , "bar-volume-foreground-5 = #f5a70a"
        , "bar-volume-foreground-6 = #ff5555"
        , "bar-volume-gradient = false"
        , "bar-volume-indicator = |"
        , "bar-volume-indicator-font = 2"
        , "bar-volume-fill = ─"
        , "bar-volume-fill-font = 2"
        , "bar-volume-empty = ─"
        , "bar-volume-empty-font = 2"
        , "bar-volume-empty-foreground = ${colors.foreground-alt}"
        ]
    -- }}}
    -- {{{
    battery =
        [ "[module/battery]"
        , "type = internal/battery"
        , "battery = " ++ bat
        , "adapter = ACAD"
        , "full-at = 98"

        , "format-charging = <animation-charging> <label-charging>"
        , "format-charging-underline = #ffb52a"

        , "format-discharging = <ramp-capacity> <label-discharging>"
        , "format-discharging-underline = ${self.format-charging-underline}"

        , "format-full-prefix = "
        , "format-full-prefix-foreground = ${colors.foreground-alt}"
        , "format-full-underline = ${self.format-charging-underline}"

        , "ramp-capacity-0 = "
        , "ramp-capacity-1 = "
        , "ramp-capacity-2 = "
        , "ramp-capacity-foreground = ${colors.foreground-alt}"

        , "animation-charging-0 = "
        , "animation-charging-1 = "
        , "animation-charging-2 = "
        , "animation-charging-foreground = ${colors.foreground-alt}"
        , "animation-charging-framerate = 750"

        , "[module/temperature]"
        , "type = internal/temperature"
        , "thermal-zone = 0"
        , "warn-temperature = 60"

        , "format = <ramp> <label>"
        , "format-underline = #f50a4d"
        , "format-warn = <ramp> <label-warn>"
        , "format-warn-underline = ${self.format-underline}"

        , "label = %temperature%"
        , "label-warn = %temperature%"
        , "label-warn-foreground = ${colors.secondary}"

        , "ramp-0 = "
        , "ramp-1 = "
        , "ramp-2 = "
        , "ramp-foreground = ${colors.foreground-alt}"
        ]
    -- }}}
    -- {{{
    powermenu =
        [ "[module/powermenu]"
        , "type = custom/menu"

        , "expand-right = true"

        , "format-spacing = 1"

        , "label-open = "
        , "label-open-foreground = ${colors.secondary}"
        , "label-close = cancel"
        , "label-close-foreground = ${colors.secondary}"
        , "label-separator = |"
        , "label-separator-foreground = ${colors.foreground-alt}"

        , "menu-0-0 = reboot"
        , "menu-0-0-exec = menu-open-1"
        , "menu-0-1 = power off"
        , "menu-0-1-exec = menu-open-2"

        , "menu-1-0 = cancel"
        , "menu-1-0-exec = menu-open-0"
        , "menu-1-1 = reboot"
        , "menu-1-1-exec = sudo reboot"

        , "menu-2-0 = power off"
        , "menu-2-0-exec = sudo poweroff"
        , "menu-2-1 = cancel"
        , "menu-2-1-exec = menu-open-0"
        ]
    -- }}}
    -- {{{
    settings =
        [ "[settings]"
        , "screenchange-reload = true"
        , ";compositing-background = xor"
        , ";compositing-background = screen"
        , ";compositing-foreground = source"
        , ";compositing-border = over"
        ]
    -- }}}
    -- {{{
    global_wm =
        [ "[global/wm]"
        , "margin-top = 5"
        , "margin-bottom = 5"
        ]
    -- }}}

-- }}}

-- }}}
-- vim: foldmethod=marker foldlevel=0
