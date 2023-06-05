--
-- The Holy and Sacred Allusive XMonad Configurations
--
-- "they say it makes the world go round!"
--

-- Import Statements
import XMonad
import Data.Monoid ()
import System.Exit ()
import XMonad.Util.SpawnOnce ( spawnOnce )
import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioLowerVolume, xF86XK_AudioRaiseVolume, xF86XK_AudioMute, xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp, xF86XK_AudioPlay, xF86XK_AudioPrev, xF86XK_AudioNext)
import XMonad.Hooks.EwmhDesktops ( ewmh )
import Control.Monad ( join, when )
import XMonad.Hooks.ManageDocks
    ( avoidStruts, docks, manageDocks, Direction2D(D, L, R, U) )
import XMonad.Hooks.ManageHelpers ( doFullFloat, isFullscreen )
import XMonad.Layout.Spacing ( spacingRaw, Border(Border) )
import XMonad.Layout.Gaps
    ( Direction2D(D, L, R, U),
      gaps,
      setGaps,
      GapMessage(DecGap, ToggleGaps, IncGap) )
import XMonad.Layout.MouseResizableTile
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Loggers
import XMonad.Layout.Minimize
import XMonad.Actions.Minimize
import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Actions.UpdatePointer
-- Layouts
import XMonad.Layout.MultiColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Fullscreen
    ( fullscreenEventHook, fullscreenManageHook, fullscreenSupport, fullscreenFull )

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Maybe (maybeToList)
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

-- Variables
myTerminal      = "kitty"
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myClickJustFocuses :: Bool
myClickJustFocuses = False
myBorderWidth   = 0
myModMask       = mod4Mask
myAltMask       = mod1Mask
myWorkspaces    = ["1","2","3"]
myNormalBorderColor  = "#3b4252"
myFocusedBorderColor = "#abd6af"

addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

------------------------------------------------------------------------
-- KEY BINDINGS
------------------------------------------------------------------------
-- Quick run launcher

-- Bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- lock screen
    , ((modm,               xK_F1    ), spawn "betterlockscreen -l")

    -- launch rofi and dashboard
    , ((myAltMask,          xK_space     ), spawn "sh ~/.config/rofi/launchers/type-6/launcher.sh")
    , ((controlMask,        xK_space     ), spawn "dmenu_run")

    , ((modm,               xK_m     ), withFocused minimizeWindow)
    , ((modm .|. shiftMask, xK_m     ), withLastMinimized maximizeWindowAndFocus)

    -- Docking
    , ((modm,               xK_d     ), spawn "kdocker -f")

    -- My Stuff
    -- , ((modm,               xK_b       ), spawn "exec ~/bin/bartoggle")
    , ((modm,               xK_v       ), spawn "copyq toggle")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), spawn "~/bin/SteamOrKill.sh")

    -- toggle all gaps
    , ((modm .|. controlMask, xK_g), sendMessage $ ToggleGaps)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_Down     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_Up     ), windows W.focusUp  )

    -- -- Move focus to the master window
    , ((0,                  0x1008ff14), spawn "playerctl play-pause"  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm,               xK_bracketleft  ), spawn "spotifyctl previous")

    -- Swap the focused window with the previous window
    , ((modm,               xK_bracketright ), spawn "spotifyctl next")

    -- Shrink the master area
    , ((modm .|. shiftMask, xK_Up     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm .|. shiftMask, xK_Down    ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Take a screenshot
    ,((modm .|. shiftMask, xK_s     ), spawn "flameshot gui")

    -- Run the powermenu
    , ((modm .|. shiftMask, xK_q     ), spawn "sh ~/.config/rofi/powermenu/type-4/powermenu.sh")

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    -- [((m .|. modm, k), windows $ f i)
    --     | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    --     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++

    --
    -- mod-shift-{Up Arrow,Down Arrow}. Move current window to screen 1 or 2.
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_Left, xK_Right, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: Bindings related to mouse actions
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- Forces window into floating mode and is now draggable whilst holding MOD + Left
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- I honestly have no idea what this does but I aint removing it.
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- Whilst holding MOD + Right enables resizing of window by moving laterally.
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

--------------Only works when mouse bindings are default------------------------

    -- MOD + Front Side Button skips the currently playing track on spotify.
    , ((modm, 8), const $ spawn "spotifyctl next")

    -- MOD + Front Side Button restarts or goes to the previous track on spotify.
    , ((modm, 9), const $ spawn "spotifyctl previous")

--------------------------------------------------------------------------------

    -- Holding MOD + Scroll Down, Shrinks/Expands the current window down
    , ((modm, 5), const $ sendMessage Expand)
    -- Holding MOD + Scroll Up, Shrinks/Expands the current window Up
    , ((modm, 4), const $ sendMessage Shrink)
    -- Holding MOD + Shift + Scroll Down rotates the open windows
    , ((modm .|. shiftMask, 5), const $ windows W.swapUp)
    -- Holding MOD + Shift + Scroll Up rotates the open windows
    , ((modm .|. shiftMask, 4), const $ windows W.swapUp)

    ]

------------------------------------------------------------------------
-- Layouts:

-- NOTES:
-- Full layout removes all window borders, this is useful when gaming or for single window productivity where the borders are not needed to show the active window. *Borders have been removed in the latest version of this config.

myLayout = avoidStruts $ smartBorders (tiled ||| Mirror tiled ||| noBorders Full ||| threeCol ||| multiCol [1] 1 0.01 (-0.5))
  where
     threeCol = ThreeColMid nmaster delta ratio
     tiled    = Tall nmaster delta ratio
     nmaster  = 1
     ratio    = 1/2
     delta    = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Forces certain windows to float, such as steam since it launches 3 windows on startup and maximises the Friends menu when opened. To retile Steam you can press MOD + T to switch the main window back to Tiling mode. This works for all floating windows.

myManageHook = fullscreenManageHook <+> manageDocks <+> composeAll
    [ className =? "MPlayer"        --> doFloat
    , title     =? "Borkski"        --> doFloat
    , title     =? "Ben"            --> doFloat
    , title     =? "Friends List"   --> doFloat
    , title     =? "Steam - News"   --> doFloat
    , title     =? "Quit GIMP"      --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "copyq"          --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , isFullscreen --> doFullFloat
                                 ]

------------------------------------------------------------------------
-- Event handling

-- Currently not utilised.

myEventHook = mempty


------------------------------------------------------------------------
--
-- Updates pointer position when tabbing between windows. Extremely useful.
--
myLogHook = updatePointer (0.5, 0.5) (0, 0)

------------------------------------------------------------------------
--
-- Spawns startup applications and processes.
--
myStartupHook = do
  spawn "xsetroot -cursor_name left_ptr"
  spawnOnce "nitrogen --restore"
  spawnOnce "nvidia-settings --load-config-only"
  spawnOnce "picom -b"
  spawnOnce "xrandr --output DP-2 --dpi 100"
  spawnOnce "dunst"
  spawnOnce "zsh ~/.config/polybar/launch.sh --forest"
  spawnOnce "emacs --daemon"
  spawnOnce "flatpak run im.riot.Riot"
  spawnOnce "flameshot"
  spawnOnce "steam -silent"
  spawnOnce "copyq"
  spawnOnce "alarm-clock-applet --hidden"
  spawnOnce "flatpak run net.agalwood.Motrix"
-- The mxw commands set my mouse keys on my Glorious Model D Wireless.
  spawnOnce "mxw config bind back key code BracketRight"
  spawnOnce "mxw config bind forward key code BracketLeft"
  spawnOnce "polkit-dumb-agent"
-- Ingore this its just a personal app I run with Brave (my browser)
  spawnOnce "/opt/brave-bin/brave --profile-directory=Default --app-id=bkhniolgpcpeomkjmokfdpkionammijb"
------------------------------------------------------------------------
--
-- Allows "xprop -root" to output current layout information.
--
myLayoutInfo :: PP
myLayoutInfo = def
    { ppSep             = " - "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap "[" "]"
    , ppVisible         = wrap "[" "]"
    }
------------------------------------------------------------------------
--
-- Super important stuff i guess.
--
main :: IO ()
main = xmonad
     . ewmh
     . fullscreenSupport
     . docks
     . withEasySB (statusBarProp "" (pure myLayoutInfo)) defToggleStrutsKey
     $ myConfig

------------------------------------------------------------------------
--
-- Outlines what each section of this config does.
--
myConfig = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        manageHook = myManageHook, 
        layoutHook = minimize . BW.boringWindows $ gaps [(L,30), (R,30), (U,40), (D,40)] $ spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True $ smartBorders $ myLayout,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook >> addEWMHFullscreen
    }

-- | I haven't done much here so just ignore it.
help :: String
help = unlines ["The default modifier key is 'super'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "alt-Space        Launch rofi",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-RightBracket  Swap the focused window with the next window",
    "mod-Shift-LeftBracket  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-Left  Shrink the master area",
    "mod-Right  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{Up,Down}        Switch focus to monitor 1 or 2",
    "mod-Shift-{Up,Down}  Move window to monitor 1 or 2",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
