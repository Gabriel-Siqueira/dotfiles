-- import {{{
import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import System.Exit
import XMonad.Actions.CycleWS
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.IM
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.Circle
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Fullscreen
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run

import XMonad.Hooks.EwmhDesktops as Ewmh
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
-- }}}

-- main {{{
main = xmonad 
    =<< statusBar myBar myPP toggleStrutsKey
    (addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys
    myConfig)
-- Command to launch the bar. {{{
myBar = "xmobar"
-- }}}
-- Custom PP, determines what is being written to the bar. {{{
myPP = xmobarPP { ppVisible = xmobarColor "#2E9AFE" "" . wrap "[" "]"
                , ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                , ppTitle = xmobarColor "#9900cc" ""
                , ppLayout = xmobarColor "#990000" "" . (\ x -> case x of
                          "ResizableTall"        -> "[|]"
                          "Mirror ResizableTall" -> "[-]"
                          "Circle"               -> "[o]"
                          "Grid"                 -> "[+]"
                          "Full"                 -> "[ ]"
                          _                      -> x )
                , ppUrgent  = xmobarColor "red" "" . wrap "*" "*"
                }
-- }}}
-- use the EWMH hints to tell panel applications about its workspaces {{{
-- and the windows therein.
myConfig = ewmh myConfig_par{ handleEventHook =
    handleEventHook myConfig_par <+> Ewmh.fullscreenEventHook }
-- }}}
-- Key binding to toggle the gap for the bar. {{{
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (myModMask, xK_b)
-- }}}
-- }}}
-- Basic Configuration {{{
baseConfig           = desktopConfig
myTerminal           = "terminator"
myMenu               = "rofi -show run -font 'Michroma 15' -location 1 -columns 5 -lines 1 -width 100 -color-enable -color-window '#222222,#222222,#00ff00' -opacity '50' -separator-style 'solid' -color-normal '#222222, #eeeeee,#222222,#444444,#eeeeee'" 
myShiftMenu          = "dmenu"
myShiftTerminal      = "terminology"
myCtrlTerminal       = "terminator --profile=Fish"
myScreenShot         = "xfce4-screenshot" 
myModMask            = mod4Mask -- (super/win key)
myNormalBorderColor  = "#94b8b8"
myFocusedBorderColor = "#0033cc"
myClickJustFocuses   = True
myFocusFollowsMouse  = False
myConfig_par = baseConfig
    { terminal           = myTerminal
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , clickJustFocuses   = myClickJustFocuses
    , focusFollowsMouse = myFocusFollowsMouse
    , layoutHook         = myLayouts
    , manageHook         = myManageHook
    , startupHook        = myStartupHook
    }
-- }}}
-- Workspace on a grid corresponding to number Pad keys {{{
myWorkspaces = ["1","2","3","4","5","6","7","8","9","0",
    ".",    "dev",    "mail",
    "aux",  "default","web",
    "midia","vm",     "docs",
    "game"]
startupWorkspace = "default"
-- }}}
-- aplly onWorkspace "<WS>" <Layout> to use customs layouts to specifics workspace {{{
myLayouts = defaultLayouts
defaultLayouts = avoidStruts(
      -- ResizableTall layout has a large master window on the left,
      -- and remaining windows tile on the right. By default each area
      -- takes up half the screen, but you can resize using "super-h" and
      -- "super-l".
      ResizableTall 1 (3/100) (1/2) []

      -- Full layout makes every window full screen. When you toggle the
      -- active window, it will bring the active window to the front.
      ||| noBorders Full

      -- Mirrored variation of ResizableTall. In this layout, the large
      -- master window is at the top, and remaining windows tile at the
      -- bottom of the screen. Can be resized as described above.
      ||| Mirror (ResizableTall 1 (3/100) (1/2) []))

      -- ThreeColMid layout puts the large master window in the center
      -- of the screen. As configured below, by default it takes of 3/4 of
      -- the available space. Remaining windows tile to both the left and
      -- right of the master window. You can resize using "super-h" and
      -- "super-l".
      -- ||| ThreeColMid 1 (3/100) (3/4)

      -- Circle layout places the master window in the center of the screen.
      -- Remaining windows appear in a circle around it
      ||| Circle

      -- Grid layout tries to equally distribute windows in the available
      -- space, increasing the number of columns and rows as necessary.
      -- Master window is at top left.
      ||| Grid
-- }}}
-- Scratchpads for some applications {{{
scratchpads =
    [ NS "spotify" "spotify" (className =? "Spotify") nonFloating
    , NS "clementine" "clementine" (className =? "Clementine") nonFloating
    ]
-- }}}
-- Keys 
-- {{{
-- {{{
numPadKeys = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"
      , "<KP_Home>", "<KP_Up>",    "<KP_Page_Up>"
      , "<KP_Left>", "<KP_Begin>", "<KP_Right>"
      , "<KP_End>",  "<KP_Down>",  "<KP_Page_Down>"
      , "<KP_Insert>"
      ]
-- }}}-
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction -- {{{
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe "zenity --text-info --font=terminus"
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()
-- }}}
-- {{{
myKeys conf =
    subtitle "lanch terminal": mkNamedKeymap conf
-- {{{
    [ ("M-<Return>",   spawnh $ XMonad.terminal conf)
    , ("M-S-<Return>", spawnh myShiftTerminal)
    , ("M-C-<Return>", spawnh myCtrlTerminal)] ++
-- }}}
    subtitle "launch application menus": mkNamedKeymap conf
-- {{{
    [ ("M-d",   spawnh myMenu)
    , ("M-S-d", spawnh myShiftMenu)] ++
-- }}}
    subtitle "layout": mkNamedKeymap conf
-- {{{
    [ ("M-<Space>", addName "Rotate through the available layout algorithms" $ sendMessage NextLayout)
    , ("M-S-<Space>", addName "Reset the layouts on the current workspace to default" $ setLayout $ XMonad.layoutHook conf)] ++
-- }}}
    subtitle "move/focus": mkNamedKeymap conf
-- {{{
    [ ("M-t", addName "push window back into tiling" $ withFocused $ windows . W.sink)
    , ("M-<Backspace>", addName "close focused window" $ kill)
    , ("M-,", addName "Increment the number of windows in the master area" $ sendMessage (IncMasterN 1))
    , ("M-.", addName "Decrement the number of windows in the master area" $ sendMessage (IncMasterN (-1)))
    , ("M-s", addName "Move to worspace on the Right" $ nextWS)
    , ("M-a", addName "Move to worspace on the Left" $ prevWS)
    , ("M-<Tab>", addName "Move focus to the next window" $ windows W.focusDown)
    , ("M-j", addName "Move focus to the next window" $ windows W.focusDown)
    , ("M-S-<Tab>", addName "Move focus to the previous window" $ windows W.focusUp  )
    , ("M-k", addName "Move focus to the previous window" $ windows W.focusUp  )
    , ("M-m", addName "Move focus to the master window" $ windows W.focusMaster)
    , ("M-u", addName "Focus on urgent workspace" $ focusUrgent)
    , ("M-S-m", addName "Swap the focused window and the master window" $ windows W.swapMaster)
    , ("M-S-j", addName "Swap the focused window with the next window" $ windows W.swapDown  )
    , ("M-S-k", addName "Swap the focused window with the previous window" $ windows W.swapUp    )] ++
-- }}}
    subtitle "resize": mkNamedKeymap conf
-- {{{
    [ ("M-n", addName "Resize viewed windows to the correct size" $ refresh)
    , ("M-h", addName "Shrink the master area" $ sendMessage Shrink)
    , ("M-l", addName "Expand the master area" $ sendMessage Expand)
    , ("M-S-h", addName "rezize on ResizableTall" $ sendMessage MirrorShrink)
    , ("M-S-l", addName "rezize on ResizableTall" $ sendMessage MirrorExpand)] ++
-- }}}
    subtitle "Quit/lock/etc": mkNamedKeymap conf
-- {{{
    [ ("M-S-c", addName "Quit xmonad" $ io (exitWith ExitSuccess))
    , ("M-c", addName "Restart xmonad" $ spawn "xmonad --recompile; xmonad --restart")
    , ("M-<KP_Delete>", addName "Lock screen" $ spawn "i3lock -i ~/Dropbox/Pictures/lock_und_dm/rsz_1maxresdefault.png")] ++
-- }}}
    subtitle "volume/brightness/etc": mkNamedKeymap conf
-- {{{
    [ ("M-<Up>", addName "Volume Up" $ spawn "amixer set Master 5%+")
    , ("M-<Down>", addName "Volume Down" $ spawn "amixer set Master 5%-")
    , ("M-<F8>", addName "enable/disable sound" $ spawn "amixer sset Master toggle")
    , ("M-<Left>", addName "Brightness Down" $ spawn "xbacklight -dec 1")
    , ("M-<Right>", addName "Brightness Up" $ spawn "xbacklight -inc 1")
    , ("<Print>", addName "Screenshot" $ spawn myScreenShot)
    ] ++
-- }}}
    subtitle "scratchpads": mkNamedKeymap conf
-- {{{
    [ ("M-<F2>", addName "popup Spotfy" $ namedScratchpadAction scratchpads "spotify")
    , ("M-<F3>", addName "popup Clementine" $ namedScratchpadAction scratchpads "clementine") ]
-- }}}
    ++
    subtitle "workspace": mkNamedKeymap conf
    -- {{{
    [("M-" ++ m ++ k, addName (n ++ i) $ windows $ f i)
        | (i, k) <- zip myWorkspaces numPadKeys
        , (f, m, n) <- [(W.greedyView, "", "switch to workspace "), (W.shift, "S-", "move client to workspace ")]]
    -- }}}
    ++
    subtitle "screens": mkNamedKeymap conf
-- {{{
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [("M-" ++ m ++ key, addName (n ++ show sc) $ screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip ["w", "e", "r"] [0..]
        , (f, m, n) <- [(W.view, "", "switch to screen "), (W.shift, "S-", "move client to screen ")]]
    where
        spawnh cmd' = addName cmd' $ spawn cmd'
-- }}}
-- }}}
-- }}}
-- ManagerHook {{{
myManageHook = namedScratchpadManageHook scratchpads <+> managerHooke_par
    where
        managerHooke_par = composeAll . concat $
            [[ className =? "MPlayer"   --> doFloat]
            , [className =? "Gimp"      --> doFloat]
            , [className =? "VirtualBox"--> doFloat]
            , [className =? x           --> doShift      "dev"| x <- cShiftDev]
            , [className =? x           --> doShift      "mail" | x <- cShiftMail]
            , [className =? x           --> doShift      "default" | x <- cShiftDefault]
            , [className =? x           --> doShift      "web" | x <- cShiftWeb]
            , [className =? x           --> doShift      "midia" | x <- cShiftMidia]
            , [className =? x           --> doShift      "vM"   | x <- cShiftVM]
            , [className =? x           --> doShift      "game" | x <- cShiftGame]
            , [className =? x           --> doShift      "docs" | x <- cShiftDocs]
            ]
                where
                cShiftDev     = ["Emacs"]
                cShiftMail    = ["Thunderbird","TelegramDesktop","Franz"]
                cShiftDefault = ["Firefox"]
                cShiftWeb     = ["Chromium","google-chrome","vivaldi-stable"]
                cShiftMidia   = ["kdenlive","Vlc","Kodi"]
                cShiftVM      = ["VirtualBox"]
                cShiftGame    = ["Steam","Mainwindow.py","Minetest"]
                cShiftDocs    = ["libreoffice","libreoffice-startcenter","libreoffice-writer","libreoffice-calc","libreoffice-impress","libreoffice-draw","libreoffice-math","libreoffice-base"]
-- }}}
-- StartupHook {{{
myStartupHook = do
                setDefaultCursor xC_center_ptr
                spawns ["dropbox", "megasync", "firefox", "stalonetray", "wicd-client --tray", "xcompmgr -n", "~/bin/random_wallpaper.sh", "unclutter -grab &", "redshift-gtk","/opt/franz-bin/Franz"]
                windows $ W.greedyView startupWorkspace
                where
                  spawns y = case y of []     -> return ()
                                       (x:xs) -> do 
                                           spawn x
                                           spawns xs
-- }}}
