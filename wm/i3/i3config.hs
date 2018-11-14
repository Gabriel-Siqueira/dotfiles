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
polyFile         = case pc of
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
---{{{
bar              = case pc of
                        "GAMa"    -> ["exec_always --no-startup-id $HOME/bin/polybar.sh"]
                        "GOLi"    -> i3bar
                        "ic"      -> i3bar
                        _         -> i3bar
--}}}
--{{{
myWallpaper      = case pc of
                        "GAMa"    -> "blue_circle.jpg"
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
myTerminal         =  case pc of
                        "GAMa"    -> "termite"
                        "GOLi"    -> "termite"
                        "ic"      -> "urxvt"
                        _         -> "i3-sensible-terminal"
-- }}}
-- {{{
mySTerminal     =  case pc of
                        "GAMa"    -> "terminator"
                        "GOLi"    -> "gnome-terminal"
                        "ic"      -> "xfce4-terminal"
                        _         -> "i3-sensible-terminal"
-- }}}
--{{{
myMenu             = case pc of
                        "GAMa"     -> "\"rofi -show run\""
                        "GOLi"     -> "\"rofi -show run\""
                        "ic"       -> "dmenu_run"
                        _          -> "dmenu_run"
--}}}
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
                                "ic"      -> "#111111"
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
                                "ic"      -> "#111111"
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
        where
                all = basic ++ keys ++ modes ++ colors ++ bar ++ fixPlace ++ autoStart ++ start

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
-- general {{{
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
        -- reshape
        -- {{{
        , ("$mod+r","mode \"reshape\"")
        -- }}}
        -- split containers
        -- {{{
        -- split in vertical orientation
        , ("$mod+v","split v")
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
        -- Print screen
        , ("Print","exec " ++ myScreenShot)
        -- volume
        , ("$mod+F5",              "exec amixer set Master 5%+")
        , ("XF86AudioRaiseVolume", "exec amixer set Master 5%+")
        , ("$mod+F6",              "exec amixer set Master 5%-")
        , ("XF86AudioLowerVolume", "exec amixer set Master 5%-")
        , ("XF86AudioMute",        "exec amixer set Master toggle")
        -- brightness
        , ("XF86MonBrightnessDown","exec xbacklight -dec 1")
        , ("$mod+F7",              "exec xbacklight -dec 1")
        , ("XF86MonBrightnessUp",  "exec xbacklight -inc 1")
        , ("$mod+F8",              "exec xbacklight -inc 1")
        -- media
        , ("XF86AudioPlay",  "exec playerctl play")
        , ("XF86AudioPause", "exec playerctl pause")
        , ("XF86AudioNext",  "exec playerctl next")
        , ("XF86AudioPrev",  "exec playerctl previous")
        , ("XF86AudioStop",  "exec playerctl stop")
        -- }}}
        -- bar 
        -- {{{
        -- bar toggle, hide or show
        , ("$mod+b","bar mode toggle")
        -- }}}
        ]
-- }}}
-- close {{{
close =
        [ ("$mod+BackSpace","kill")
        ]
-- }}}
-- focus {{{
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
        , ("$mod+Shift+p", "focus child")
        , ("$mod+u", "[urgent=latest] focus")
        , ("$mod+m", "mark swap")
        , ("$mod+Shift+space", "[con_mark=\"swap\"] focus")
        ]
-- }}}
-- move {{{
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
-- workspaces {{{
workspaces =
    -- back and forth
    [("$mod+space", "workspace back_and_forth")] ++
    [("$mod+x", "move workspace to output right")] ++
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
-- scratchpad {{{
scratchpad =
        [ ("$mod+f", "[instance=\"file\"] scratchpad show; [instance=\"file\"] move position center; mode \"sp-file\"")
        -- , ("$mod+c", "[instance=\"math\"] scratchpad show; [instance=\"math\"] move position center; mode \"sp-math\"")
        , ("$mod+n", "[instance=\"simplenote\"] scratchpad show; [instance=\"simplenote\"] move position center; mode \"sp-note\"")
        , ("$mod+t", "[instance=\"ster\"] scratchpad show; [instance=\"ster\"] move position center; mode \"sp-term\"")
        -- , ("$mod+m", "[instance=\"mail\"] scratchpad show; [instance=\"mail\"] move position center; mode \"sp-mail\"")
        ]
-- }}}

-- }}}
-- Modes {{{

modes = [ "mode \"reshape\" {"] ++ addKeys reshape ++ addKeys focus ++ addKeys move ++ ["}"] ++
        ["mode \"sp-note\" {"]  ++ addKeys spNote ++ addKeys scratchpad' ++ ["}"] ++
        ["mode \"sp-file\" {"] ++ addKeys spFile ++ addKeys scratchpad' ++ ["}"] ++
        ["mode \"sp-term\" {"] ++ addKeys spTerm ++ addKeys scratchpad' ++ ["}"]
        where
        -- reshape {{{
        reshape =
            [ ("h",     "resize shrink width  5 px or 5 ppt")
            , ("j",     "resize grow   height 5 px or 5 ppt")
            , ("k",     "resize shrink height 5 px or 5 ppt")
            , ("l",     "resize grow   width  5 px or 5 ppt")
            , ("Left",  "resize shrink width  5 px or 5 ppt")
            , ("Down",  "resize grow   height 5 px or 5 ppt")
            , ("Up",    "resize shrink height 5 px or 5 ppt")
            , ("Right", "resize grow   width  5 px or 5 ppt")
            , ("Escape","mode \"default\"")
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
            , ("Shift+p",       "focus child")
            -- back to normal
            , ("Escape",  "mode \"default\"")
            ]
        -- }}}
        -- spMath {{{
        spMath = 
            [ ("$mod+c", "[instance=\"math\"] focus; [instance=\"math\"] scratchpad show; mode \"default\"")
            , ("$mod+s", "[instance=\"math\"] focus") 
            ]
        -- }}}
        -- spNote {{{
        spNote = 
            [ ("$mod+n", "[instance=\"simplenote\"] focus; [instance=\"simplenote\"] scratchpad show; mode \"default\"")
            , ("$mod+s", "[instance=\"simplenote\"] focus") 
            ]
        -- }}}
        -- spFile {{{
        spFile = 
            [ ("$mod+f", "[instance=\"file\"] focus; [instance=\"file\"] scratchpad show; mode \"default\"")
            , ("$mod+s", "[instance=\"file\"] focus") 
            ]
        -- }}}
        -- spTerm {{{
        spTerm = 
            [ ("$mod+t", "[instance=\"ster\"] focus; [instance=\"ster\"] scratchpad show; mode \"default\"")
            , ("$mod+s", "[instance=\"ster\"] focus") 
            ]
        -- }}}
        -- spMail {{{
        spMail = 
            [ ("$mod+m", "[instance=\"mail\"] focus; [instance=\"mail\"] scratchpad show; mode \"default\"")
            , ("$mod+s", "[instance=\"mail\"] focus") 
            ]
        -- }}}
        -- scratchpad {{{
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
        , "client.focused_inactive  #aaaaaa #555555 #ffffff #111111 #111111"
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
        , "        inactive_workspace " ++ inactiveWorkspaceBorder ++ " " ++ inactiveWorkspaceBackground ++ "    #999900"
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
        win = "[instance=\"^"
-- {{{
        fix =
            [ (ass, "game",  ["Steam","Mainwindow.py","Minetest"])
            , (ass, "midi", ["Vlc","Kodi","Spotify"])
            , (ass, "virM", ["VirtualBox"])
            , (ass, "docs", ["libreoffice","libreoffice-startcenter","libreoffice-writer","libreoffice-calc","libreoffice-impress","libreoffice-draw","libreoffice-math","libreoffice-base"])
            , (ass, "deft", ["Emacs"])
            , (ass, "auxE", ["Firefox","qutebrowser"])
            , (ass, "auxD", ["Chromium","google-chrome","vivaldi-stable", "Opera"])
            , (ass, "read", ["calibre"])
            , (ass, "deve", ["Eclipse"])
            , (ass, "mail", ["thunderbird","TelegramDesktop","Franz","Inboxer"])
            ]
-- }}}
-- {{{
        sp = [ ("file", ["move scratchpad","floating enable","resize set 1100 600"])
             , ("simplenote", ["move scratchpad","floating enable","resize set 1200 700"])
             , ("ster", ["move scratchpad","floating enable","resize set 1100 600"])
             -- , ("math", ["move scratchpad","floating enable","resize set 800  300"])
             -- , ("mail", ["move scratchpad","floating enable","resize set 1200 700"])
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
                , "megasync"
                , "redshift-gtk"
                , "alarm-clock-applet"
                , "nm-applet"
                -- , "wicd-client --tray"
                , "twmnd"
                , "~/bin/bat.sh"
                , "feh --bg-fill ~/Dropbox/Pictures/mywallpaper/" ++ myWallpaper
                , "compton"
                , "emacs"
                , "qutebrowser"
                , term_lauch ++ "ster -e tmux"
                , term_lauch ++ "file -e ranger"
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

-- }}}
-- vim: foldmethod=marker foldlevel=0 
-- Local Variables:
-- origami-fold-style: triple-braces
-- End:
