-- import {{{

import System.Exit
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Circle
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.ResizableTile
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation hiding (Swap)
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Layout.Simplest

import XMonad.Hooks.EwmhDesktops as Ewmh
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- }}}

-- main {{{

main = xmonad 
    =<< (statusBar myBar myPP toggleStrutsKey
        $ addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys
        $ withNavigation2DConfig def
        myConfig)

-- Command to launch the bar. {{{
myBar = "xmobar"
-- }}}
-- Custom PP, determines what is being written to the bar. {{{
myPP = xmobarPP { ppVisible = xmobarColor colorVisible "" . wrap "[" "]"
                , ppCurrent = xmobarColor colorCurrent "" . wrap "[" "]"
                , ppTitle = xmobarColor colorTitle ""
                , ppLayout = xmobarColor colorLayout "" . (\ x -> case x of
                        "BSP"                         -> "[T]"
                        "ResizableTall"               -> "[|]"
                        "Mirror ResizableTall"        -> "[-]"
                        "Circle"                      -> "[o]"
                        "Grid"                        -> "[+]"
                        "Full"                        -> "[ ]"
                        "Tabbed BSP"                  -> "t[T]"
                        "Tabbed ResizableTall"        -> "t[|]"
                        "Tabbed Mirror ResizableTall" -> "t[-]"
                        "Tabbed Circle"               -> "t[o]"
                        "Tabbed Grid"                 -> "t[+]"
                        "Tabbed Full"                 -> "t[ ]"
                        _                             -> x )
                , ppUrgent  = xmobarColor colorUrgent "" . wrap "*" "*"
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
myMenu               = "rofi -matching fuzzy -show run -font 'Michroma 15' -location 1 -columns 5 -lines 1 -width 100 -color-enable -color-window '#222222,#222222,#00ff00' -opacity '50' -separator-style 'solid' -color-normal '#222222, #eeeeee,#222222,#444444,#eeeeee'" 
myShiftMenu          = "dmenu"
myShiftTerminal      = "terminology"
myCtrlTerminal       = "terminator --profile=Fish"
myScreenShot         = "xfce4-screenshot" 
myModMask            = mod4Mask -- (super/win key)
myClickJustFocuses   = True
myFocusFollowsMouse  = False
myConfig_par = baseConfig
    { terminal           = myTerminal
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , clickJustFocuses   = myClickJustFocuses
    , focusFollowsMouse  = myFocusFollowsMouse
    , layoutHook         = myLayouts
    , manageHook         = myManageHook
    , startupHook        = myStartupHook
    }

-- }}}
-- Theme {{{

colorBase = "black"
colorTitle = "#9900cc"
colorLayout = "#990000"
colorCurrent = "yellow"
colorVisible = "#2E9AFE"
colorUrgent = "red"
myNormalBorderColor  = "#94b8b8"
myFocusedBorderColor = "#0033cc"
myTabTheme = def
    { fontName              = "terminus"
    , activeColor           = colorBase
    , inactiveColor         = colorBase 
    , activeBorderColor     = colorCurrent
    , inactiveBorderColor   = myNormalBorderColor
    , activeTextColor       = colorTitle
    , inactiveTextColor     = colorTitle
    }

-- }}}
-- Workspace {{{

myWorkspaces = ["1","2","3","4","5","6","7","8","9","0",
    ".",    "dev",    "mail",
    "aux",  "default","web",
    "midia","vm",     "docs",
    "game"]
startupWorkspace = "default"

-- }}}
-- Layouts {{{
-- aplly onWorkspace "<WS>" <Layout> to use customs layouts to specifics workspace

myLayouts = windowNavigation $ addTabs shrinkText myTabTheme $ subLayout [] Simplest $ defaultLayouts
defaultLayouts = avoidStruts(
    -- Split focus windon in Half
    emptyBSP

    -- Full layout makes every window full screen. When you toggle the
    -- active window, it will bring the active window to the front.
    ||| noBorders Full

    -- ResizableTall layout has a large master window on the left,
    -- and remaining windows tile on the right. By default each area
    -- takes up half the screen, but you can resize using "super-h" and
    -- "super-l".
    ||| ResizableTall 1 (3/100) (1/2) []

    -- Mirrored variation of ResizableTall. In this layout, the large
    -- master window is at the top, and remaining windows tile at the
    -- bottom of the screen. Can be resized as described above.
    ||| Mirror (ResizableTall 1 (3/100) (1/2) []))

    -- Tabbed Layout
    ||| tabbed shrinkText myTabTheme

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
-- Keys {{{

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
    , ("M-S-<Space>", addName "Reset the layouts on the current workspace to default" $ setLayout $ XMonad.layoutHook conf)
    , ("M-M1-u", addName "umarge layout" $ withFocused (sendMessage . UnMerge))
    , ("M-M1-h", addName "marge layout left" $  sendMessage $ pullGroup L)
    , ("M-M1-j", addName "marge layout down" $  sendMessage $ pullGroup D)
    , ("M-M1-k", addName "marge layout up" $  sendMessage $ pullGroup U)
    , ("M-M1-l", addName "marge layout right" $  sendMessage $ pullGroup R)
    , ("M-M1-<Left>", addName "marge layout left" $  sendMessage $ pullGroup L)
    , ("M-M1-<Down>", addName "marge layout down" $  sendMessage $ pullGroup D)
    , ("M-M1-<Up>", addName "marge layout up" $  sendMessage $ pullGroup U)
    , ("M-M1-<Right>", addName "marge layout right" $  sendMessage $ pullGroup R) ] ++
-- }}}
    subtitle "move": mkNamedKeymap conf
-- {{{
    [ ("M-t", addName "push window back into tiling" $ withFocused $ windows . W.sink)
    , ("M-<Backspace>", addName "close focused window" $ kill)
    , ("M-,", addName "Increment the number of windows in the master area" $ sendMessage (IncMasterN 1))
    , ("M-.", addName "Decrement the number of windows in the master area" $ sendMessage (IncMasterN (-1)))
    , ("M-S-s", addName "Swap brothers" $ sendMessage Swap)
    , ("M-s", addName "Rotate split (hor,ver)" $ sendMessage Rotate)
    , ("M-S-h", addName "windows move left" $ windowSwap L False)
    , ("M-S-j", addName "windows move down" $ windowSwap D False)
    , ("M-S-k", addName "windows move up" $ windowSwap U False)
    , ("M-S-l", addName "windows move right" $ windowSwap R False)
    , ("M-S-<Left>", addName "windows move left" $ windowSwap L False)
    , ("M-S-<Down>", addName "windows move down" $ windowSwap D False)
    , ("M-S-<Up>", addName "windows move up" $ windowSwap U False)
    , ("M-S-<Right>", addName "windows move right" $ windowSwap R False)
    , ("M-S-m", addName "Swap the focused window and the master window" $ windows W.swapMaster)
    , ("M-S-C-<Tab>", addName "Swap the focused window with the next window" $ windows W.swapDown  )
    , ("M-C-<Tab>", addName "Swap the focused window with the previous window" $ windows W.swapUp    )] ++
-- }}}
    subtitle "focus": mkNamedKeymap conf
-- {{{
    [ ("M-<Tab>", addName "Move focus to the next window" $ windows W.focusDown)
    , ("M-S-<Tab>", addName "Move focus to the previous window" $ windows W.focusUp  )
    , ("M-m", addName "Move focus to the master window" $ windows W.focusMaster)
    , ("M-u", addName "Focus on urgent workspace" $ focusUrgent)
    , ("M-h", addName "windows focus left" $ windowGo L False)
    , ("M-j", addName "windows focus down" $ windowGo D False)
    , ("M-k", addName "windows focus up" $ windowGo U False)
    , ("M-l", addName "windows focus right" $ windowGo R False)
    , ("M-<Left>", addName "windows focus left" $ windowGo L False)
    , ("M-<Down>", addName "windows focus down" $ windowGo D False)
    , ("M-<Up>", addName "windows focus up" $ windowGo U False)
    , ("M-<Right>", addName "windows focus right" $ windowGo R False)] ++
-- }}}
    subtitle "resize": mkNamedKeymap conf
-- {{{
    [ ("M-n", addName "Resize viewed windows to the correct size" $ refresh)
    , ("M-C-h", addName "windows expand left" $ sendMessage $ ExpandTowards L)
    , ("M-C-j", addName "windows expand down" $ sendMessage $ ExpandTowards D)
    , ("M-C-k", addName "windows expand up" $ sendMessage $ ExpandTowards U)
    , ("M-C-l", addName "windows expand right" $ sendMessage $ ExpandTowards R)
    , ("M-C-<Left>", addName "windows expand left" $ sendMessage $ ExpandTowards L)
    , ("M-C-<Down>", addName "windows expand down" $ sendMessage $ ExpandTowards D)
    , ("M-C-<Up>", addName "windows expand up" $ sendMessage $ ExpandTowards U)
    , ("M-C-<Right>", addName "windows expand right" $ sendMessage $ ExpandTowards R)
    , ("M-C-S-h", addName "windows shrink left" $ sendMessage $ ShrinkFrom L)
    , ("M-C-S-j", addName "windows shrink down" $ sendMessage $ ShrinkFrom D)
    , ("M-C-S-k", addName "windows shrink up" $ sendMessage $ ShrinkFrom U)
    , ("M-C-S-l", addName "windows shrink right" $ sendMessage $ ShrinkFrom R)
    , ("M-C-S-<Left>", addName "windows shrink left" $ sendMessage $ ShrinkFrom L)
    , ("M-C-S-<Down>", addName "windows shrink down" $ sendMessage $ ShrinkFrom D)
    , ("M-C-S-<Up>", addName "windows shrink up" $ sendMessage $ ShrinkFrom U)
    , ("M-C-S-<Right>", addName "windows shrink right" $ sendMessage $ ShrinkFrom R)
    , ("M--", addName "Shrink the master area" $ sendMessage Shrink)
    , ("M-+", addName "Expand the master area" $ sendMessage Expand)
    , ("M-S--", addName "rezize on ResizableTall" $ sendMessage MirrorShrink)
    , ("M-S-+", addName "rezize on ResizableTall" $ sendMessage MirrorExpand)] ++
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
-- vim: ft=haskell:foldmethod=marker
