--{{{ Imports
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
    
import XMonad.Hooks.EwmhDesktops as Ewmh
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
--}}}

baseConfig = desktopConfig

-- main function
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

--{{{ Xmobar
-- Command to launch the bar.
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppVisible = xmobarColor "#2E9AFE" "" . wrap "[" "]"
                , ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                , ppTitle = xmobarColor "#9900cc" ""
                , ppLayout = xmobarColor "#990000" ""
                , ppUrgent  = xmobarColor "red" "" . wrap "*" "*"
                }
          
 -- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)      
--}}}
                                                     
-- use the EWMH hints to tell panel applications about its workspaces
-- and the windows therein.
myConfig = ewmh myConfig_par{ handleEventHook =
            handleEventHook myConfig_par <+> Ewmh.fullscreenEventHook }
-- my changes on baseConfig
myConfig_par = baseConfig
    { terminal           = myTerminal
    , modMask            = myModMask
    , keys               = myKeys

    , workspaces         = myWorkspaces

    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor

    , layoutHook         = myLayouts                     
    , manageHook         = myManageHook 
    , startupHook        = myStartupHook
    }

myTerminal           = "xfce4-terminal"
myModMask            = mod4Mask
myNormalBorderColor  = "#94b8b8"
myFocusedBorderColor = "#0033cc"

                       -- Workspace on a grid corresponding to number Pad keys 
myWorkspaces = [
    "..",    "Dev",    "Mail",
    "aux➊",  "standard","aux➋",
    "Midia",  "VM",      ".",
    "Game"
  ]
startupWorkspace = "standard"

-- aplly onWorkspace "<WS>" <Layout> to use customs layouts to specifics workspace
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

  -- Mirrored variation of ResizableTall. In this layout, the large
  -- master window is at the top, and remaining windows tile at the
  -- bottom of the screen. Can be resized as described above.
  ||| Mirror (ResizableTall 1 (3/100) (1/2) []))

numKeys = [
--{{{ Keys
    xK_7, xK_8, xK_9
  , xK_4, xK_5, xK_6
  , xK_1, xK_2, xK_3
  , xK_0
  ]
numPadKeys = [
  xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up,
  xK_KP_Left, xK_KP_Begin, xK_KP_Right,
  xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down,
  xK_KP_Insert
  ]
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm              , xK_Return), spawn $ XMonad.terminal conf)
    , ((modm .|. shiftMask, xK_Return), spawn "terminator")
    -- launch dmenu
    , ((modm,               xK_d     ), spawn "dmenu_run")
    -- launch gmrun
    , ((modm .|. shiftMask, xK_d     ), spawn "gmrun")
    -- close focused window
    , ((modm .|. shiftMask, xK_q     ), kill)
     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
    -- Move to worspace on the Left/Right
    , ((modm,               xK_s     ), nextWS)
    , ((modm,               xK_a     ), prevWS)
    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster)
    -- Focus on urgent workspce
    , ((myModMask, xK_u), focusUrgent)
    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_m     ), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_c     ), io (exitWith ExitSuccess))
    -- Restart xmonad
    , ((modm              , xK_c     ), spawn "xmonad --recompile; xmonad --restart")
    -- Volume
    , ((modm              , xK_Up    ), spawn "amixer set Master 5%+")
    , ((modm              , xK_Down  ), spawn "amixer set Master 5%-")
    , ((modm              , xK_F8    ), spawn "amixer sset Master toggle")
    -- Brightness
    , ((modm              , xK_Left  ), spawn "xbacklight -dec 1")
    , ((modm              , xK_Right ), spawn "xbacklight -inc 1")
    -- Screenshot
    , ((0                 , xK_Print ), spawn "xfce4-screenshot")
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. myModMask, k), windows $ f i)
        | (i, k) <- zip myWorkspaces numPadKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. myModMask, k), windows $ f i)
        | (i, k) <- zip myWorkspaces numKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m)    <- [(W.view, 0), (W.shift, shiftMask)]]
--}}}


myManageHook = (composeAll . concat $
--{{{
    [[ className =? "MPlayer"   --> doFloat]
    , [className =? "Gimp"      --> doFloat]
    , [className =? "VirtualBox"--> doFloat]
    , [className =? x           --> doShift      "Mail" | x <- cShiftMail]
    , [className =? x           --> doShift      "VM"   | x <- cShiftVM]
    , [className =? x           --> doShift      "Game" | x <- cShiftGame]
    , [className =? x           --> doShift      "Dev"| x <- cShiftDev]
    , [className =? x           --> doShift      "Midia" | x <- cShiftMidia]
    ]
  ) where
  cShiftMail  = ["Thunderbird","Telegram"]
  cShiftGame  = ["Steam", "PlayOnLinux", "Minetest"]
  cShiftDev   = ["Emacs"]
  cShiftVM    = ["VirtualBox"]
  cShiftMidia = ["kdenlive","Vlc","Spotify"]
  -- doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws
--}}}


myStartupHook = do
--{{{
                setDefaultCursor xC_center_ptr
                spawns ["dropbox","thunderbird","stalonetray","wicd-client --tray","xcompmgr -n","~/bin/random_wallpaper.sh","~/applications/Telegram/Telegram"]
                windows $ W.greedyView startupWorkspace
                where
                  spawns y = case y of []      -> return ()
                                       (x:xs) -> do
                                                spawn x
                                                spawns xs
--}}}
