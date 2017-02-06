import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.ManageHelpers
import XMonad
import System.Exit

import XMonad.Actions.Submap
import XMonad.Actions.CopyWindow
import XMonad.Layout.Tabbed
import XMonad.Layout.Circle
import XMonad.Layout.ThreeColumns
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Prompt
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.Input
import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Layout.Named
import XMonad.Layout.Minimize
import XMonad.Hooks.Minimize
import XMonad.Hooks.SetWMName

import XMonad.Layout.Spiral
import XMonad.Util.Scratchpad

import Graphics.X11.Xlib
import Graphics.X11.Xinerama
-- import Graphics.X11.Xlib.Extras
-- import Graphics.X11.Xlib.Event

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified XMonad.Layout.BoringWindows as B

import Data.List
import Data.Function

myTerminal = "xterm -sh 1.001"

myBorderWidth = 1

myNormalBorderColor = "#222222"
myFocusedBorderColor = "#aa0000"

myModMask = mod4Mask
myNumLockMask = mod2Mask

myWorkspaces = ["1:web", "2:im", "3:code", "4:terminals"] ++ map show [5..7] ++ ["wifi", "music"]

myLayout = avoidStruts $
           tiled
           ||| Mirror tiled
           ||| Full
           ||| tabbed shrinkText defaultTheme
           ||| threeCol
--           ||| spiral (4/3)
  where
    tiled = Tall nmaster delta ratio

    threeCol = ThreeCol nmaster delta ratio

    nmaster = 1

    ratio = 1/2

    delta = 2/100

myManageHook = composeAll
    [  isFullscreen             --> doFullFloat
     , className =? "Vncviewer" --> doFloat
     , className =? "Gimp"      --> doFloat
     , className =? "MPlayer"   --> doFloat
     , resource  =? "desktop_window" --> doFloat
     , title     =? "zsh-onetime" --> doFloat
     , manageDocks
     , scratchpadManageHook (W.RationalRect 0.125 0.25 0.75 0.5)
    ]

myStartupHook = do
  setWMName "LG3D"
  spawn "conky --daemonize -b -c ~/.config/conky/conky1.conf"

myLogHook xmproc = 
      dynamicLogWithPP xmobarPP
        { 
           ppOutput = hPutStrLn xmproc
         , ppTitle = xmobarColor "green" "" . shorten 50
        }

myKeys conf@XConfig {XMonad.modMask = mod} = M.fromList $
   [
    --((mod, xK_s), namedScratchpadAction scratchpads "browser")
    -- minimize current window
    --  ((mod,               xK_m     ), withFocused minimizeWindow)
    -- restore next window
    --, ((mod .|. shiftMask, xK_m     ), sendMessage RestoreNextMinimizedWin)
    -- open terminal scratchpad
    --, ((0, xK_F12), namedScratchpadAction scratchpads "terminal")
      ((mod .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
      --exit
    , ((mod .|. shiftMask, xK_q), io exitSuccess)
    , ((mod, xK_a), sendMessage ToggleStruts)
    -- launch file manager
    , ((mod, xK_f), spawn "thunar")
    -- go to coresponding workspace
    , ((mod .|. shiftMask, xK_f), windows $ W.greedyView "5:fm")
    , ((mod .|. shiftMask, xK_s), windows $ W.greedyView "6:work")
    , ((mod .|. shiftMask, xK_d), windows $ W.greedyView "7:math")
    -- open window selection menu
    --, ((mod, xK_o), goToSelected defaultGSConfig)
    -- launch command prompt
    , ((mod, xK_p     ), spawn "dmenu_run")
    -- launch screensaver
    , ((controlMask .|. shiftMask , xK_l), spawn "slock")
    --close current window
    , ((mod .|. shiftMask, xK_c     ), kill1)
    , ((mod,               xK_space ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((mod .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((mod,               xK_n     ), refresh)
    -- Move focus to the next window
    , ((mod,              xK_Tab    ), windows W.focusDown)
    -- Move focus to the next window
    , ((mod,               xK_j     ), windows W.focusDown)
    -- Move focus to the previous window
    , ((mod,               xK_k     ), windows W.focusUp)
    -- Move focus to the master window
    , ((mod .|. shiftMask,   xK_Tab ), windows W.focusMaster  )
    -- Swap the focused window and the master window
    , ((mod,               xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((mod .|. shiftMask, xK_j     ), windows W.swapDown  )
    -- Swap the focused window with the previous window
    , ((mod .|. shiftMask, xK_k     ), windows W.swapUp    )
    -- Shrink the master area
    , ((mod,               xK_h     ), sendMessage Shrink)
    -- Expand the master area
    , ((mod,               xK_l     ), sendMessage Expand)
    -- Push window back into tiling
    , ((mod,               xK_t     ), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((mod              , xK_comma ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((mod              , xK_period), sendMessage (IncMasterN (-1)))
    --, ((mod              , xK_Right), moveTo Next (WSIs notSP))
    --, ((mod             , xK_Left), moveTo Prev (WSIs notSP))
    , ((mod              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    --send window to etc
    , ((mod .|. shiftMask, xK_a), windows $ W.shift "9:etc")
    , ((mod .|. shiftMask, xK_h), swapPrevScreen)
    , ((mod .|. shiftMask, xK_l), swapNextScreen)
 ]

 ++  
 [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
  ,((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
	,((0, xF86XK_MonBrightnessUp), spawn "xbacklight +17")
	,((0, xF86XK_MonBrightnessDown), spawn "xbacklight -17")
	,((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2-")
	,((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2+")
	,((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
	,((0, xF86XK_AudioPlay), spawn "clementine --play-pause")
	,((0, xF86XK_AudioPrev), spawn "clementine --previous")
	,((0, xF86XK_AudioNext), spawn "clementine --next")
	,((mod4Mask, xK_s), spawn "dmenu_extended_run \" -> Internet search: \" \"Google\"")
 ]
 ++
 --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. mod, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((mod .|. mask, key), f sc)
        | (key, sc) <- zip [xK_q, xK_w, xK_e] [0..]
        , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]

 
main = do 
  xmproc <- spawnPipe "xmobar /home/grapefroot/.xmobarrc"
  xmonad $ defaults xmproc

defaults xmproc = defaultConfig {
  terminal = myTerminal,
  borderWidth = myBorderWidth,
  modMask = myModMask,
--  numlockMask = myNumlockMask,
  workspaces = myWorkspaces,
  normalBorderColor = myNormalBorderColor,
  focusedBorderColor = myFocusedBorderColor,

  keys = myKeys,

  startupHook = myStartupHook,
  layoutHook = myLayout,
  manageHook = myManageHook,
  logHook = myLogHook xmproc
 }
