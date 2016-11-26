import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Hooks.SetWMName
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.ManageHelpers
 
myManageHook = composeAll
    [  isFullscreen             --> doFullFloat
     , className =? "Vncviewer" --> doFloat
     , className =? "Gimp"      --> doFloat
    ]
 
main = do
    xmproc <- spawnPipe "xmobar  /home/grapefroot/.xmobarrc"
    xmonad $ defaultConfig
        { startupHook =  
        	 	do setWMName "LG3D"
		 	   spawn "conky --daemonize -b"
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
	,((0, xF86XK_MonBrightnessUp), spawn "xbacklight +17")
	,((0, xF86XK_MonBrightnessDown), spawn "xbacklight -17")
	,((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2-")
	,((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2+")
	,((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
	,((0, xF86XK_AudioPlay), spawn "clementine --play-pause")
	,((0, xF86XK_AudioPrev), spawn "clementine --previous")
	,((0, xF86XK_AudioNext), spawn "clementine --next")


	,((mod4Mask, xK_s), spawn "dmenu_extended_run \" -> Internet search: \" \"Google\"")]
	

