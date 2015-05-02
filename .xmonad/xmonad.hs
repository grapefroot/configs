import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Hooks.SetWMName
import Graphics.X11.ExtraTypes.XF86
 
myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    ]
 
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar  /home/grapefroot/.xmobarrc"
    xmonad $ defaultConfig
        { startupHook = setWMName "LG3D"
	, manageHook = manageDocks <+> myManageHook -- make sure to include myManageHook definition from above
                        <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        ,((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        ,((0, xK_Print), spawn "scrot")
	,((0, xF86XK_KbdBrightnessUp), spawn "xbacklight +10")
	,((0, xF86XK_KbdBrightnessDown), spawn "xbacklight -10")
	,((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2-")
	,((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2+")
	,((0, xF86XK_AudioMute), spawn "amixer -D pulse set Master toggle")]

