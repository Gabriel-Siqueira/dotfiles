import System.IO
import System.Directory (doesDirectoryExist, getHomeDirectory)
import System.FilePath (joinPath)
import Control.Monad (when)

-- Config {{{

pc = "GENe"
-- wallpaper {{{
myWallpaper      = case pc of
                        "GENe"    -> "green_circle.jpg"
                        "GAMa"    -> "blue_circle.jpg"
                        "GOLi"    -> "flower.jpg"
                        "ic"      -> "wood.jpg"
                        _         -> "clover.jpg"
--}}}
-- workspaces {{{
startupWorkspace = w_d
w_g  = "game"
w_mi = "midi"
w_v  = "VirM"
w_do = "docs"
w_ae = "auxE"
w_d  = "deft"
w_ad = "auxD"
w_r  = "read"
w_de = "deve"
w_ma = "mail"
ws = [
    w_g,
    w_mi,w_v,w_do,
    w_ae,w_d,w_ad,
    w_r,w_de,w_ma,
    "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
--}}}
-- applications {{{

-- terminal {{{
myTerminal         =  case pc of
                        "GENe"    -> "termite"
                        "GAMa"    -> "termite"
                        "GOLi"    -> "termite"
                        "ic"      -> "urxvt"
                        _         -> "i3-sensible-terminal"
-- }}}
-- shift terminal {{{
mySTerminal     =  case pc of
                        "GENe"    -> "terminator"
                        "GAMa"    -> "terminator"
                        "GOLi"    -> "gnome-terminal"
                        "ic"      -> "xfce4-terminal"
                        _         -> "i3-sensible-terminal"
-- }}}
-- menu {{{
myMenu             = case pc of
                        "GENe"     -> "\"rofi -show run\""
                        "GAMa"     -> "\"rofi -show run\""
                        "GOLi"     -> "\"rofi -show run\""
                        "ic"       -> "dmenu_run"
                        _          -> "dmenu_run"
--}}}
-- shift menu {{{
mySmenu             = case pc of
                        "GENe"     -> "morc_menu"
                        "GAMa"     -> "dmenu_run"
                        "GOLi"     -> "dmenu_run"
                        "ic"       -> "dmenu_run"
                        _          -> "dmenu_run"
--}}}
myScreenShot       = "gnome-screenshot -a"

-- }}}
-- bar {{{
bar = case pc of
    "GENe"    -> ["exec_always --no-startup-id $HOME/bin/polybar.sh"]
    "GAMa"    -> ["exec_always --no-startup-id $HOME/bin/polybar.sh"]
    "GOLi"    -> i3bar
    "ic"      -> i3bar
    _         -> i3bar
--}}}

-- }}}
-- Main {{{

main = do
        home <- getHomeDirectory
        writeFile (i3File home) . unlines $ all
        where
            i3File h = joinPath [h, ".config/i3/config"]
            all = basic ++ keys ++ modes ++ colors ++ bar ++ fixPlace ++ autoStart ++ start

-- }}}
-- Basic Config {{{

basic =
        [ "font pango:DejaVu Sans Mono 0"
        -- Use Mouse+$mod to drag floating windows to their wanted position
        , "floating_modifier $mod"
        -- keyboard layout
        , "exec setxkbmap -layout us,br -variant dvp, -option \"grp:alt_space_toggle\""
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
        , ("$mod+a",      "exec \"rofi -show drun\"")
        , ("$mod+o",      "exec \"locate home | rofi -matching regex  -dmenu -i -p \'locate\' | xargs -r -0 xdg-open\"")
        , ("$mod+w",      "exec \"rofi -lines 0 -dmenu -i -p \'url\' | xargs -r -0 qutebrowser\"; workspace " ++ w_ae)
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
        , ("XF86MonBrightnessDown","exec xbacklight -dec 5")
        , ("$mod+F7",              "exec xbacklight -dec 1")
        , ("XF86MonBrightnessUp",  "exec xbacklight -inc 5")
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
        , ("$mod+Shift+Tab", "[con_mark=\"swap\"] focus")
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
    [("$mod+Tab", "workspace back_and_forth")] ++
    -- move output
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
        , "    status_command i3status"
        , "    position top"
        , "    workspace_buttons yes"
        , "    binding_mode_indicator yes"
        , "    font pango:Terminus 11px"
        , "    colors{"
        , "        background #111111"
        , "        statusline #eeeeee"
        , "        separator  #666666"
          --       <colorclass>            <border>           <background>       <text>
        , "        focused_workspace  " ++ "#111111" ++ " " ++ "#111111" ++ "    #ffff00"
        , "        active_workspace   " ++ "#111111" ++ " " ++ "#111111" ++ "    #ffff00"
        , "        inactive_workspace " ++ "#111111" ++ " " ++ "#111111" ++ "    #999900"
        , "        urgent_workspace   " ++ "#111111" ++ " " ++ "#111111" ++ "    #ff0000"
        , "        binding_mode       " ++ "#111111" ++ " " ++ "#111111" ++ "    #ff5500"
        , "    }"
        , "}"
        ]

-- }}}
-- Fix Place/Float {{{
fixPlace = concat $ fixWp ++ fixSp ++ fixFl
        where
        fixWp = map (\(sel, wp, cs) -> map (\c -> "for_window " ++ sel ++ c ++ "$\"] move to workspace " ++ wp) cs) fix
        fixSp = map (\(ins, coms) -> map (\com -> "for_window " ++ win ++ ins ++ "$\"] " ++ com) coms) sp
        fixFl = map (\(sel, cs) -> map (\c -> "for_window " ++ sel ++ c ++ "$\"] foating enable") cs) float
        ass = "[class=\"^"
        win = "[instance=\"^"
-- {{{
        fix =
            [ (ass, "game",  ["Steam","Mainwindow.py","Minetest"])
            , (ass, "midi", ["Vlc","Kodi","Spotify","Lollypop"])
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
-- {{{
        float =
          [ (ass, ["Pavucontrol"])
          ]
-- }}}


-- }}}
-- Autostart {{{

autoStart = map (\x -> if x == "" then "" else "exec " ++ x) autoStart'
        where
        autoStart' =
                [ case pc of
                        "GENe"    -> "dropbox"
                        "GAMa"    -> "dropbox"
                        "GOLi"    -> "dropbox"
                        "ic"      -> "~/.dropbox-dist/dropboxd"
                        _         -> ""
                -- , "megasync"
                , "redshift-gtk"
                , "alarm-clock-applet --hidden"
                , "nm-applet"
                , "xfce4-power-manager"
                , "twmnd"
                , "feh --bg-fill ~/Dropbox/Pictures/mywallpaper/" ++ myWallpaper
                , "compton"
                , "emacs"
                , "qutebrowser"
                , "chromium"
                , "simplenote"
                , term_lauch ++ "ster -e tmux"
                , term_lauch ++ "file -e ranger"
                ]
        term_lauch = case pc of
                    "GENe"    -> "termite --name="
                    "GAMa"    -> "termite --name="
                    "GOLi"    -> "termite --name="
                    "ic"      -> "urxvt -name "
                    _         -> "urxvt -name "

-- }}}
-- Startup workspace {{{

start = ["exec --no-startup-id i3-msg workspace " ++ startupWorkspace]

-- }}}
-- vim: foldmethod=marker foldlevel=0
-- Local Variables:
-- origami-fold-style: triple-braces
-- End:
