{-# LANGUAGE FlexibleContexts #-}

import XMonad
import XMonad.Util.EZConfig
import XMonad.Actions.Navigation2D
import XMonad.Actions.CycleWS
import XMonad.Actions.SwapWorkspaces
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.SubLayouts
import XMonad.Layout.Simplest
import XMonad.Layout.WindowNavigation
import XMonad.Layout.BoringWindows
import XMonad.Layout.TrackFloating
import XMonad.Layout.Spacing
import XMonad.Hooks.UrgencyHook (focusUrgent, withUrgencyHook, NoUrgencyHook(..))
import XMonad.Hooks.DynamicLog (PP(..), wrap, dynamicLogWithPP)
import XMonad.Hooks.ManageDocks (docks, avoidStruts, ToggleStruts(ToggleStruts))
import XMonad.Hooks.FadeWindows (fadeWindowsEventHook)
import XMonad.Hooks.InsertPosition (insertPosition, Position(..), Focus(..))
import XMonad.Hooks.RefocusLast (refocusLastLayoutHook, refocusLastWhen, isFloat, toggleFocus)
import XMonad.Util.NamedScratchpad (NamedScratchpad(NS), customFloating, namedScratchpadAction, namedScratchpadManageHook)
import XMonad.Util.Dmenu (dmenu)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.XUtils (fi)
import XMonad.Util.WindowProperties (getProp32)

import Data.Monoid (All(..))
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import System.Exit (exitSuccess)
import System.Posix.Unistd (getSystemID, nodeName)
import System.IO (Handle, hPutStrLn)

import qualified Data.Map as M
import qualified XMonad.StackSet as W

myTerminal :: String
myTerminal = "kitty -o allow_remote_control=yes --single-instance --listen-on unix:@mykitty"

termLaunch :: String -> String -> String
termLaunch name prog = myTerminal ++ " --name=" ++ name ++ " " ++ prog

myMenu :: String
-- myMenu = "rofi -show run"
myMenu = "ulauncher"

myLock :: String
myLock = "i3lock -t -i ~/Dropbox/Pictures/lock_und_dm/guide_to_the_galaxy.png"

myStatusBar :: String
myStatusBar = "~/bin/polybar.sh"

myScreenShot :: String
myScreenShot = "import `date +%s.png`"

myWorkspaces :: [String]
myWorkspaces = numWorkspaces ++ otherWorkspaces
numWorkspaces :: [String]
numWorkspaces = map show [0..9::Int]
otherWorkspaces :: [String]
otherWorkspaces = [midi, book, docs, auxE, deft, auxD, virM, deve, mail, free, game, call]
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
free :: String
free = "."
call :: String
call = "call"
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
  let conf = withUrgencyHook NoUrgencyHook . docks $ (def {
      terminal = "termite"
    , keys = layoutkeys
    , modMask = mod4Mask
    , layoutHook = myLayoutsHook
    , startupHook = myStartupHook host
    , workspaces= myWorkspaces
    , handleEventHook = myHandleEventHook
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#000000"
    , borderWidth = 2
    , manageHook = myManageHook
    , focusFollowsMouse = False
    , logHook = myLogHook handle
    } `additionalKeysP` keyMaps)
  xmonad conf

layoutkeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
layoutkeys conf@XConfig {XMonad.modMask = myModMask} = M.fromList [((myModMask, xK_space ), sendMessage NextLayout) , ((myModMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)]

keyMaps :: [(String, X ())]
keyMaps = programKeys ++ menuKeys ++ quitRealoadKeys ++ multiMediaKeys ++ moveKeys ++ focusKeys ++ resizeKeys ++ moreLayoutKeys ++ workspacesKeys ++ miscKeys
  where
    programKeys = [
        ("M-<Return>", spawn myTerminal)
      , ("<Print>", spawn myScreenShot)
      , ("M-f", toggleScratchpad "file")
      , ("M-t", toggleScratchpad "ster") ]
    menuKeys = [
        ("M-d", spawn myMenu)
      , ("M-a", spawn "rofi -show drun")
      , ("M-o", spawn "locate home | rofi -matching regex  -dmenu -i -p \'locate\' | xargs -r -0 xdg-open")
      , ("M-w", spawn "~/bin/url.sh")
      , ("M-m", spawn "~/bin/macros.sh")
      ]
    quitRealoadKeys = [
        ("M-M1-r", spawn "xmonad --restart")
      , ("M-M1-e", quit)
      , ("M-M1-l", spawn myLock)
      , ("M-M1-s", spawn "systemctl suspend")
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
      , ("M-S-x", swapNextScreen)
      , ("M-S-h", windowSwap L False)
      , ("M-S-j", windowSwap D False)
      , ("M-S-k", windowSwap U False)
      , ("M-S-l", windowSwap R False)
      , ("M-S-<Left>", windowSwap L False)
      , ("M-S-<Down>", windowSwap D False)
      , ("M-S-<Up>", windowSwap U False)
      , ("M-S-<Right>", windowSwap R False)
      , ("M-S-m", windows W.swapMaster)
      , ("M-S-p", windows W.swapUp)
      , ("M-S-n", windows W.swapDown)
      , ("M-M1-t", withFocused $ windows . W.sink)]
    focusKeys = [
        ("M-u", focusUrgent)
      , ("M-x", nextScreen)
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
      , ("M-r s", toggleWindowSpacingEnabled)
      , ("M-r u", withFocused (sendMessage . UnMerge))
      , ("M-r m", withFocused (sendMessage . MergeAll))
      , ("M-r h", sendMessage $ pullGroup L)
      , ("M-r j", sendMessage $ pullGroup D)
      , ("M-r k", sendMessage $ pullGroup U)
      , ("M-r l", sendMessage $ pullGroup R)
      , ("M-r <Left>", sendMessage $ pullGroup L)
      , ("M-r <Down>", sendMessage $ pullGroup D)
      , ("M-r <Up>", sendMessage $ pullGroup U)
      , ("M-r <Right>", sendMessage $ pullGroup R)
      , ("M-r <Space>", toSubl NextLayout)]
    workspacesKeys = [
        ("M-" ++ s ++ k, windows $ f i)
        | (i, k) <- zip myWorkspaces (map show [0..9::Int] ++ ["<Insert>", "<Home>", "<Page_Up>", "<Delete>", "<End>", "<Page_Down>","M1-<Insert>","M1-<Home>","M1-<Page_Up>","M1-<Delete>","M1-<End>","M1-<Page_Down>"]) ++
                   zip myWorkspaces ["*","(",")","}","+","{","]","[","!","=","<KP_Home>","<KP_Up>","<KP_Page_Up>","<KP_Left>","<KP_Begin>","<KP_Right>","M1-<KP_Home>","M1-<KP_Up>","M1-<KP_Page_Up>","M1-<KP_Left>","M1-<KP_Begin>","M1-<KP_Right>"]
        , (f, s) <- [(W.view, ""), (W.shift, "S-"),(swapWithCurrent, "s ")]] ++
      [ ("M-<Tab>", toggleWS' ["NSP"]) ]
    miscKeys = [
        ("M-c", spawn "~/bin/clock.sh")
      , ("M-S-t", spawn "~/bin/theme.sh")
      , ("M-b", sendMessage ToggleStruts) ]
    quit = do
      s <- dmenu ["yes","no"]
      when (s == "yes") (io exitSuccess)
    toggleScratchpad com = do
      maybe_win <- gets (W.peek . windowset)
      flt <- case maybe_win of
        Nothing -> return False
        Just win -> runQuery isFloat win
      when flt toggleFocus
      namedScratchpadAction myScratchpads com

myLayoutsHook = refocusLastLayoutHook . avoidStruts . windowNavigation . trackFloating .
                useTransientFor .  addTabs shrinkText tabTheme .
                subLayout [0] (Simplest ||| tiled ||| Mirror tiled) .  boringWindows $
                (Full ||| space tiled ||| (space . Mirror $ tiled))
 where
     space l = spacingRaw False (Border 0 0 0 0) False (Border 5 5 5 5) False l
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
            , "/usr/lib/geoclue-2.0/demos/agent" -- necessary to redshift
            -- , "alarm-clock-applet --hidden"
            , "nm-applet"
            -- , "wicd-gtk --tray"
            -- , "netctl-tray"
            -- , "cmst -m -w 5"
            , "xfce4-power-manager"
            -- , "feh --bg-fill ~/Dropbox/Pictures/mywallpaper/" ++ myWallpaper host
            -- , "variety"
            , "nohup python3 /usr/local/bin/WeatherDesk > /dev/null &"
            , "compton"
            -- , "emacs"
            , "qutebrowser"
            , "vivaldi-stable"
            -- , "chromium"
            -- , "brave"
            , "dunst"
            -- , termLaunch "main_term" "tmux new -A -s standard"
            ]
  windows $ W.greedyView startupWorkspace
  where
    spawns y = case y of
      [] -> return ()
      (x:xs) -> do {spawnOnce x; spawns xs}

myHandleEventHook :: Event -> X All
myHandleEventHook = handleEventHook def
                <+> refocusLastWhen isFloat
                <+> fadeWindowsEventHook
                <+> fullscreenEventHook

myManageHook :: ManageHook
myManageHook = insertPosition Below Newer <+> namedScratchpadManageHook myScratchpads <+> (composeAll . concat $
            [ [appName   =? "main_term"        --> doShift deft]
            , [appName   =? "git-gui--askpass" --> doFloat]
            , [className =? "MPlayer"          --> doFloat]
            , [className =? "Gimp"             --> doFloat]
            , [className =? "VirtualBox"       --> doFloat]
            , [className =? "pavucontrol"      --> doFloat]
            , [role      =? "task_dialog"      --> doFloat]
            , [role      =? "pop-up"           --> doFloat]
            , [className =? x                  --> doShift midi | x <- cShiftMidi]
            , [className =? x                  --> doShift virM | x <- cShiftVirM]
            , [className =? x                  --> doShift deft | x <- cShiftDeft]
            , [className =? x                  --> doShift auxD | x <- cShiftAuxD]
            , [className =? x                  --> doShift auxE | x <- cShiftAuxE]
            , [className =? x                  --> doShift book | x <- cShiftBook]
            , [className =? x                  --> doShift game | x <- cShiftGame]
            , [className =? x                  --> doShift docs | x <- cShiftDocs]
            , [className =? x                  --> doShift mail | x <- cShiftMail]
            , [className =? x                  --> doShift deve | x <- cShiftDeve]
            , [className =? x                  --> doShift call | x <- cShiftCall]
            ])
              where
                cShiftDeve = ["Eclipse"]
                cShiftCall = ["zoom","discord","Slack"]
                cShiftMail = ["thunderbird","TelegramDesktop","Franz","Inboxer"]
                cShiftDeft = ["Emacs"]
                cShiftAuxD = ["Chromium","google-chrome","vivaldi-stable", "Opera", "Brave-browser"]
                cShiftMidi = ["Kodi", "Vlc", "Spotify", "Kodi", "Lollypop"]
                cShiftVirM = ["VirtualBox"]
                cShiftGame = ["Steam","Mainwindow.py","Minetest", "Lutris"]
                cShiftAuxE = ["Firefox", "qutebrowser"]
                cShiftBook = ["calibre"]
                cShiftDocs = ["libreoffice","libreoffice-startcenter","libreoffice-writer","libreoffice-calc","libreoffice-impress","libreoffice-draw","libreoffice-math","libreoffice-base"]
                role = stringProperty "WM_WINDOW_ROLE"

myScratchpads :: [NamedScratchpad]
myScratchpads =
    [ NS "file" (termLaunch "file" "ranger") (appName =? "file") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    , NS "ster" (termLaunch "ster" "tmux new -A -s scratchpad") (appName =? "ster") (customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2))
    ]

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
      , ppUrgent = wrap "%{F#ff3333}" "%{F-}"
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
            "Tabbed Spacing Simplest"             -> "[=]"
            "Tabbed Spacing BSP"                  -> "[T]"
            "Tabbed Spacing ResizableTall"        -> "[|]"
            "Tabbed Spacing Mirror ResizableTall" -> "[-]"
            "Tabbed Spacing Circle"               -> "[o]"
            "Tabbed Spacing Grid"                 -> "[+]"
            "Tabbed Spacing Full"                 -> "[" ++ show n_windows ++ "]"
            "Tabbed Spacing Tabbed Simplest"      -> "[=]"
            _                             -> x )
      }

fullscreenEventHook :: Event -> X All
fullscreenEventHook (ClientMessageEvent _ _ _ dpy win typ (action:dats)) = do
  wmstate <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  wstate <- fromMaybe [] `fmap` getProp32 wmstate win

  let isFull = fromIntegral fullsc `elem` wstate
      add = 1
      toggle = 2

  when (typ == wmstate && fi fullsc `elem` dats) $ do
    when (action == add || (action == toggle && not isFull)) $ do
      sendMessage ToggleStruts
      sendMessage ToggleStruts

  return $ All True
fullscreenEventHook _ = return $ All True
