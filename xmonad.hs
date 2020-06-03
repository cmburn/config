import XMonad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.Gaps
import XMonad.Hooks.EwmhDesktops

defaults = defaultConfig {
    normalBorderColor = "#ffde7a",
    focusedBorderColor = "#ffcc33",
    manageHook = manageDocks <+> manageHook defaultConfig,
    layoutHook = myLayout,
    startupHook = myStartupHook,
    logHook = ewmhDesktopsLogHook,
    handleEventHook = ewmhDesktopsEventHook,
    borderWidth = 2
    }


myStartupHook = ewmhDesktopsStartup <+> do
                                        spawn "pkill xfce4-panel"
                                        spawn "sleep 1 && xfce4-panel --disable-wm-check"
    

myLayout = gaps [(L, 34)] $ avoidStrutsOn [(L)] $ spacingWithEdge 13 $ spiral (610/987) 

myKeys = [ 
    ((mod1Mask .|. controlMask, xK_Return ), spawn "xfce4-terminal"),
    ((mod4Mask, xK_f), spawn "firefox"),
    ((mod4Mask, xK_t), spawn "thunderbird"),
    ((mod1Mask, xK_question), spawn "rofi -combi-modi window,drun,ssh -theme solarized -font 'Spleen 32x64 16' -show combi -icon-theme 'Papirus' -show-icons"),
    ((mod1Mask .|. shiftMask, xK_slash), spawn "rofi -combi-modi window,drun,ssh -theme solarized -font 'Spleen 32x64 16' -show combi -icon-theme 'Papirus' -show-icons"),
    ((mod1Mask .|. controlMask, xK_Delete), spawn "xflock4"),
    ((mod1Mask .|. controlMask, xK_x), kill),
    ((mod1Mask, xK_F11), spawn "sndioctl output.level=+.0625"),
    ((mod1Mask, xK_F10), spawn "sndioctl output.level=-.0625"),
    ((mod1Mask, xK_F9), spawn "sndioctl output.mute=!")
    ]

main = do
    xmonad $ defaults `additionalKeys` myKeys
