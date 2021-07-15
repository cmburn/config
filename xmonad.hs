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
    logHook = ewmhDesktopsLogHook,
    handleEventHook = ewmhDesktopsEventHook,
    borderWidth = 2
    }


myLayout = gaps [(U, 34)] $ avoidStrutsOn [(U)] $ spacingWithEdge 13 $ spiral (610/987)

myKeys = [
    ((mod1Mask .|. controlMask, xK_Return ), spawn "kitty"),
    ((mod4Mask, xK_f), spawn "firefox"),
    ((mod4Mask, xK_t), spawn "thunderbird"),
    ((mod1Mask, xK_question), spawn "rofi -combi-modi window,drun,ssh -theme solarized -font 'SF Mono Medium 16' -show combi -icon-theme 'Papirus' -show-icons"),
    ((mod1Mask .|. shiftMask, xK_slash), spawn "rofi -combi-modi window,drun,ssh -theme solarized -font 'SF Mono Medium 16' -show combi -icon-theme 'Papirus' -show-icons"),
    ((mod1Mask .|. controlMask, xK_x), kill),
    ((mod4Mask , xK_r), spawn "rofi -combi-modi window,drun,ssh -theme solarized -font 'SF Mono Medium 16' -show combi -icon-theme 'Papirus' -show-icons"),
    ((mod1Mask, xK_F11), spawn "sndioctl output.level=+.0625"),
    ((mod1Mask, xK_F10), spawn "sndioctl output.level=-.0625"),
    ((mod1Mask, xK_F9), spawn "sndioctl output.mute=!")
    ]

main = do
    xmonad $ defaults `additionalKeys` myKeys
