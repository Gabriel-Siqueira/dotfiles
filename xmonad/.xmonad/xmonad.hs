{-# LANGUAGE FlexibleContexts #-}

import XMonad
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Actions.Navigation2D (windowGo, Direction2D(..), windowSwap)
import XMonad.Actions.CycleWS (toggleWS')
import XMonad.Actions.SwapWorkspaces (swapWithCurrent)
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.Tabbed (shrinkText, Theme(..), addTabs)
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.SubLayouts (GroupMsg(UnMerge,MergeAll), pullGroup, onGroup, subLayout)
import XMonad.Layout.Simplest (Simplest(Simplest))
import XMonad.Layout.WindowNavigation (windowNavigation)
import XMonad.Layout.BoringWindows (boringWindows, focusUp, focusDown)
import XMonad.Hooks.UrgencyHook (focusUrgent)
import XMonad.Hooks.DynamicLog (PP(..), wrap, dynamicLogWithPP)
import XMonad.Hooks.ManageDocks (docks, avoidStruts, ToggleStruts(ToggleStruts))
import XMonad.Hooks.FadeWindows (fadeWindowsEventHook)
import XMonad.Util.NamedScratchpad (NamedScratchpad(NS), defaultFloating, customFloating, namedScratchpadAction, namedScratchpadManageHook)
import XMonad.Util.Dmenu (dmenu)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Run (spawnPipe)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
---------------------------
import Data.Monoid (All)
import Control.Monad (when)
import System.Exit (exitSuccess)
import System.Posix.Unistd (getSystemID, nodeName)
import System.IO (Handle, hPutStrLn)
---------------------------
import qualified Data.Map as M
import qualified XMonad.StackSet as W

myTerminal :: String
myTerminal = "termite"

myMenu :: String
myMenu = "rofi -show run"

myLock :: String
myLock = "i3lock -t -i ~/Dropbox/Pictures/lock_und_dm/guide_to_the_galaxy.png"

myStatusBar :: String
myStatusBar = "~/bin/polybar.sh"

myScreenShot :: String
myScreenShot = "gnome-screenshot -a"

myWorkspaces :: [String]
myWorkspaces = numWorkspaces ++ otherWorkspaces
numWorkspaces :: [String]
numWorkspaces = map show [0..9::Int]
otherWorkspaces :: [String]
otherWorkspaces = [midi, book, docs, auxE, deft, auxD, virM, deve, mail, free1, game, free2]
midi :: String
midi = "midi"
book :: String
book = "read"
docs :: String
docs = "docs"
auxE :: String
auxE = "auxE"
deft :: String
deft = "deft"
auxD :: String
auxD = "auxD"
game :: String
game = "game"
deve :: String
deve = "deve"
mail :: String
mail = "mail"
free1 :: String
free1 = "."
free2 :: String
free2 = ".."
virM :: String
virM = "virM"

startupWorkspace :: String
startupWorkspace = deft

myWallpaper :: String -> String
myWallpaper host
  | host == "GENe" = "green_circle.jpg"
  | host == "GAMa" = "blue_circle.jpg"
  | otherwise = "yellow_circle.jpg"

main :: IO ()
main = do
  host <- fmap nodeName getSystemID
  handle <- spawnPipe myStatusBar
  let conf = docks (def {
  -- xmonad =<< statusBar myStatusBar myPP toggleStrutsKey (def {
      terminal = "termite"
    , keys = layoutkeys
    , modMask = mod4Mask
    , layoutHook = myLayoutsHook
    , startupHook = myStartupHook host
    , workspaces= myWorkspaces
    , handleEventHook = myHandleEventHook
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#000000"
    , borderWidth = 3
    , manageHook = myManageHook
    , focusFollowsMouse = False
    , logHook = myLogHook handle
    } `additionalKeysP` keyMaps)
  xmonad conf

layoutkeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
layoutkeys conf@(XConfig {XMonad.modMask = myModMask}) = M.fromList $ [((myModMask, xK_space ), sendMessage NextLayout) , ((myModMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)]

keyMaps :: [(String, X ())]
keyMaps = programKeys ++ menuKeys ++ quitRealoadKeys ++ multiMediaKeys ++ moveKeys ++ focusKeys ++ resizeKeys ++ moreLayoutKeys ++ workspacesKeys ++ miscKeys
  where
    programKeys = [
        ("M-<Return>", spawn myTerminal)
      , ("<Print>", spawn myScreenShot)
      , ("M-m", namedScratchpadAction myScratchpads "spotify")
      , ("M-f", namedScratchpadAction myScratchpads "file")
      , ("M-t", namedScratchpadAction myScratchpads "ster") ]
    menuKeys = [
        ("M-d", spawn myMenu)
      , ("M-a", spawn "rofi -show drun")
      , ("M-o", spawn "locate home | rofi -matching regex  -dmenu -i -p \'locate\' | xargs -r -0 xdg-open")
      , ("M-w", spawn "~/bin/url.sh")
      , ("M-M1-m", spawn "~/bin/macros.sh")
      ]
    quitRealoadKeys = [
        ("M-M1-r", spawn "xmonad --restart")
      , ("M-M1-e", quit)
      , ("M-M1-l", spawn myLock)
      ]
    multiMediaKeys = [
        ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
      , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
      , ("<XF86AudioMute>",        spawn "amixer set Master toggle")
      , ("<XF86MonBrightnessDown>",spawn "xbacklight -dec 5")
      , ("<XF86MonBrightnessUp>",  spawn "xbacklight -inc 5")
      , ("<XF86AudioPlay>",  spawn "playerctl play")
      , ("<XF86AudioPause>", spawn "playerctl pause")
      , ("<XF86AudioNext>",  spawn "playerctl next")
      , ("<XF86AudioPrev>",  spawn "playerctl previous")
      , ("<XF86AudioStop>",  spawn "playerctl stop")
      ]
    moveKeys = [
        ("M-<Backspace>", kill)
      , ("M-S-h", windowSwap L False)
      , ("M-S-j", windowSwap D False)
      , ("M-S-k", windowSwap U False)
      , ("M-S-l", windowSwap R False)
      , ("M-S-<Left>", windowSwap L False)
      , ("M-S-<Down>", windowSwap D False)
      , ("M-S-<Up>", windowSwap U False)
      , ("M-S-<Right>", windowSwap R False)
      , ("M-S-m", windows W.swapMaster)
      , ("M-S-n", windows W.swapUp)
      , ("M-S-p", windows W.swapDown)]
    focusKeys = [
        ("M-u", focusUrgent)
      , ("M-h", windowGo L False)
      , ("M-j", windowGo D False)
      , ("M-k", windowGo U False)
      , ("M-l", windowGo R False)
      , ("M-<Left>", windowGo L False)
      , ("M-<Down>", windowGo D False)
      , ("M-<Up>", windowGo U False)
      , ("M-<Right>", windowGo R False)
      , ("M-n", focusUp)
      , ("M-p", focusDown)
      , ("M-g t", onGroup W.focusDown')
      , ("M-g S-t", onGroup W.focusUp')
      , ("M-g M-t", onGroup W.focusDown')
      , ("M-g M-S-t", onGroup W.focusUp')]
    resizeKeys = [
        ("M-,", sendMessage Shrink)
      , ("M-.", sendMessage Expand)]
    moreLayoutKeys = [
        ("M-r ,", sendMessage (IncMasterN 1))
      , ("M-r .", sendMessage (IncMasterN (-1)))
      , ("M-r u", withFocused (sendMessage . UnMerge))
      , ("M-r m", withFocused (sendMessage . MergeAll))
      , ("M-r h", sendMessage $ pullGroup L)
      , ("M-r j", sendMessage $ pullGroup D)
      , ("M-r k", sendMessage $ pullGroup U)
      , ("M-r l", sendMessage $ pullGroup R)
      , ("M-r <Left>", sendMessage $ pullGroup L)
      , ("M-r <Down>", sendMessage $ pullGroup D)
      , ("M-r <Up>", sendMessage $ pullGroup U)
      , ("M-r <Right>", sendMessage $ pullGroup R)]
    workspacesKeys = [
        ("M-" ++ s ++ k, windows $ f i)
        | (i, k) <- zip myWorkspaces (map show [0..9::Int] ++ ["<Insert>", "<Home>", "<Page_Up>", "<Delete>", "<End>", "<Page_Down>","M1-<Insert>","M1-<Home>","M1-<Page_Up>","M1-<Delete>","M1-<End>","M1-<Page_Down>"]) ++
                   zip myWorkspaces ["*","(",")","}","+","{","]","[","!","=","<KP_Home>","<KP_Up>","<KP_Page_Up>","<KP_Left>","<KP_Begin>","<KP_Right>","M1-<KP_Home>","M1-<KP_Up>","M1-<KP_Page_Up>","M1-<KP_Left>","M1-<KP_Begin>","M1-<KP_Right>"]
        , (f, s) <- [(W.greedyView, ""), (W.shift, "S-"),(swapWithCurrent, "s ")]] ++
      [ ("M-<Tab>", toggleWS' ["NSP"]) ]
    miscKeys = [
          ("M-b", sendMessage ToggleStruts) ]
    quit = do
      s <- dmenu ["yes","no"]
      when (s == "yes") (io exitSuccess)

myLayoutsHook = avoidStruts $ windowNavigation $ addTabs shrinkText tabTheme $ subLayout [] Simplest $ boringWindows $ fullscreenFull
  (Full ||| tiled ||| Mirror tiled ||| Full)
 where
     tiled   = ResizableTall nmaster delta ratio []
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

     tabTheme = def {
         fontName              = "-misc-fixed-medium-r-semicondensed--0-0-75-75-c-0-iso8859-1"
       , activeColor           = "gray"
       , inactiveColor         = "black"
       , activeBorderColor     = "gray"
       , inactiveBorderColor   = "gray"
       , activeTextColor       = "gray"
       , inactiveTextColor     = "black"
       , decoHeight            = 15
       }

myStartupHook :: String -> X ()
myStartupHook host = do
  setDefaultCursor xC_left_ptr
  spawns [  "dropbox"
            , "redshift-gtk"
            , "alarm-clock-applet --hidden"
            , "nm-applet"
            , "xfce4-power-manager"
            , "feh --bg-fill ~/Dropbox/Pictures/mywallpaper/" ++ myWallpaper host
            , "compton"
            , "emacs"
            , "qutebrowser"
            , "chromium"
            , "dunst"
            -- , term_launch ++ "ster -e tmux"
            -- , term_launch ++ "file -e ranger"
            -- , term_launch ++ "top -e htop"
            ]
  windows $ W.greedyView startupWorkspace
  where
    spawns y = case y of
      [] -> return ()
      (x:xs) -> do {spawnOnce x; spawns xs}

myHandleEventHook :: Event -> X All
myHandleEventHook = handleEventHook def
                <+> fadeWindowsEventHook
                <+> fullscreenEventHook

myManageHook :: ManageHook
myManageHook = namedScratchpadManageHook myScratchpads <+> (composeAll . concat $
            [[ className =? "MPlayer"     --> doFloat]
            , [className =? "Gimp"        --> doFloat]
            , [className =? "VirtualBox"  --> doFloat]
            , [className =? "pavucontrol" --> doFloat]
            , [role      =? "task_dialog" --> doFloat]
            , [role      =? "pop-up"      --> doFloat]
            , [className =? x             --> doShift midi | x <- cShiftMidi]
            , [className =? x             --> doShift virM | x <- cShiftVirM]
            , [className =? x             --> doShift deft | x <- cShiftDeft]
            , [className =? x             --> doShift auxD | x <- cShiftAuxD]
            , [className =? x             --> doShift auxE | x <- cShiftAuxE]
            , [className =? x             --> doShift book | x <- cShiftBook]
            , [className =? x             --> doShift game | x <- cShiftGame]
            , [className =? x             --> doShift docs | x <- cShiftDocs]
            , [className =? x             --> doShift mail | x <- cShiftMail]
            , [className =? x             --> doShift deve | x <- cShiftDeve]
            ])
              where
                cShiftDeve = ["Eclipse"]
                cShiftMail = ["thunderbird","TelegramDesktop","Franz","Inboxer"]
                cShiftDeft = ["Emacs"]
                cShiftAuxD = ["Chromium","google-chrome","vivaldi-stable", "Opera"]
                cShiftMidi = ["Kodi", "Vlc", "Kodi", "Spotify", "Lollypop"]
                cShiftVirM = ["VirtualBox"]
                cShiftGame = ["Steam","Mainwindow.py","Minetest", "Lutris"]
                cShiftAuxE = ["Firefox", "qutebrowser"]
                cShiftBook = ["calibre"]
                cShiftDocs = ["libreoffice","libreoffice-startcenter","libreoffice-writer","libreoffice-calc","libreoffice-impress","libreoffice-draw","libreoffice-math","libreoffice-base"]
                role = stringProperty "WM_WINDOW_ROLE"

myScratchpads :: [NamedScratchpad]
myScratchpads =
    [ NS "spotify" "spotify" (className =? "Spotify") defaultFloating
    , NS "file" (term_launch ++ "file -e ranger") (appName =? "file") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    , NS "ster" (term_launch ++ "ster -e tmux") (appName =? "ster") (customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2))
    ]
  where
    term_launch = "termite --name="

myLogHook :: Handle -> X ()
myLogHook h = do
  n_windows <- length <$> gets (W.index . windowset)
  dynamicLogWithPP . myPP $ n_windows
  where
    myPP n_windows = def {
        ppOutput = hPutStrLn h
      , ppVisible = wrap "%{F#2E9AFE}[" "]%{F-}"
      , ppTitle = const ""
      , ppCurrent = wrap "%{F#fcba03}[" "]%{F-}"
      , ppUrgent = wrap "%{F#ff3333}*" "*%{F-}"
      , ppLayout = wrap "%{F#ffffff}" "%{F-}" .
        (\ x -> case x of
            "BSP"                         -> "[T]"
            "ResizableTall"               -> "[|]"
            "Mirror ResizableTall"        -> "[-]"
            "Circle"                      -> "[o]"
            "Grid"                        -> "[+]"
            "Full"                        -> "[" ++ show n_windows ++ "]"
            "Tabbed Simplest"             -> "[=]"
            "Tabbed BSP"                  -> "[T]"
            "Tabbed ResizableTall"        -> "[|]"
            "Tabbed Mirror ResizableTall" -> "[-]"
            "Tabbed Circle"               -> "[o]"
            "Tabbed Grid"                 -> "[+]"
            "Tabbed Full"                 -> "[" ++ show n_windows ++ "]"
            "Tabbed Tabbed Simplest"      -> "[=]"
            _                             -> x )
      }
