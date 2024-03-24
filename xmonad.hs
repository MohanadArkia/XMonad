-------------------------------------------------------------------------------
-- IMPORTS
-------------------------------------------------------------------------------
	-- Base
import XMonad
import Data.Monoid(mempty)
import System.Exit

	-- Keyboard
import Graphics.X11.ExtraTypes.XF86

	-- Layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
	
	-- Data
import Data.Maybe (fromJust)

	-- Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog(dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))

	-- Utilities
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.Themes
import XMonad.Util.EZConfig (additionalKeys)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

---------------------------------------------
-- VARIABLES
---------------------------------------------
myTerminal :: String
myTerminal      = "alacritty"

myBrowser :: String
myBrowser	= "brave"

myFileManager :: String
myFileManager	= "pcmanfm"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Border width
myBorderWidth :: Dimension
myBorderWidth   = 2

-- Mod Key
myModMask :: KeyMask
myModMask       = mod4Mask

-- Workspaces
myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

-- Border Colors
myNormalBorderColor :: String
myNormalBorderColor  = "#dddddd"

myFocusedBorderColor :: String
myFocusedBorderColor = "#7070ff"

-- Workspaces Colors
textColor = "#DFF6FF"
usedWorkspaceColor = "#DFF6FF"
unusedWorkspaceColor = "#DFF6FF"
underLineColor = "#4169E1"
upperLineColor = "#4169E1"
seperatorColor = "#00FFFF"
titleColor = "#06283D"

-- opened window count
windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
	
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn myTerminal)
	
    -- volume keys 
    --, ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    --, ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
    --, ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")		
    , ((0, xF86XK_AudioMute), spawn "$HOME/Documents/scripts/volume/volume.sh mute")
    , ((0, xF86XK_AudioLowerVolume), spawn "$HOME/Documents/scripts/volume/volume.sh down")
    , ((0, xF86XK_AudioRaiseVolume), spawn "$HOME/Documents/scripts/volume/volume.sh up")	

    -- brightness keys
    --, ((0, xF86XK_MonBrightnessUp), spawn "lux -a 10%")
    --, ((0, xF86XK_MonBrightnessDown), spawn "lux -s 10%")
    , ((0, xF86XK_MonBrightnessUp), spawn "$HOME/Documents/scripts/brightness/brightness.sh up")
    , ((0, xF86XK_MonBrightnessDown), spawn "$HOME/Documents/scripts/brightness/brightness.sh down")
    --
    -- Lock screen with xScreenSaver
    , ((modm .|. shiftMask, xK_x     ), spawn "xscreensaver-command -lock")
   
    -- Launch Qutebrowser
    , ((modm .|. shiftMask, xK_b     ), spawn myBrowser)
	
    -- Launch File Manager 
    , ((modm .|. shiftMask, xK_f     ), spawn myFileManager)
 	
    -- Fullscreen
    , ((modm,               xK_f     ), toggleFullscreen)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")
    
    -- launch rofi
    --, ((modm,               xK_p     ), spawn "rofi -show drun")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

    -- Flip screen
    , ((mod1Mask .|. mod4Mask, xK_Left), spawn "$HOME/Documents/scripts/FlipScreen/flip.sh left")
    , ((mod1Mask .|. mod4Mask, xK_Right), spawn "$HOME/Documents/scripts/FlipScreen/flip.sh right")
    , ((mod1Mask .|. mod4Mask, xK_Down), spawn "$HOME/Documents/scripts/FlipScreen/flip.sh inverted")
    , ((mod1Mask .|. mod4Mask, xK_Up), spawn "$HOME/Documents/scripts/FlipScreen/flip.sh normal")

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

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
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- LAYOUTS
------------------------------------------------------------------------

mySpacing = spacingRaw False            -- False=Apply even when single window
                       (Border 5 5 5 5) -- Screen border size top bot rght lft
                       True             -- Enable screen border
                       (Border 5 5 5 5) -- Window border size
                       True             -- Enable window borders

myLayout = avoidStruts $ mySpacing $ tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.

-- By default, do nothing.
myStartupHook = do
    spawnOnce "nitrogen --restore &"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.

main = do
  xmproc <- spawnPipe "xmobar $HOME/.config/xmobar/xmobarrc"  
  xmonad $ docks $ defaults {
	 logHook = dynamicLogWithPP $ def { 
		  ppOutput = hPutStrLn xmproc  
		, ppCurrent = xmobarColor textColor "" . wrap
			 ("<box type=Bottom width=2 mb=2 color=" ++ underLineColor ++ ">") "</box>"
	-- Visible but not current workspace
		, ppVisible = xmobarColor textColor "" . clickable
	-- Hidden workspace
 	        , ppHidden = xmobarColor usedWorkspaceColor "" . wrap
 		 ("<box type=Top width=2 mt=2 color=" ++ upperLineColor ++ ">") "</box>" . clickable
 	-- Hidden workspaces (no windows)
 		, ppHiddenNoWindows = xmobarColor unusedWorkspaceColor "" . clickable
 	-- Title of active window
 		, ppTitle = xmobarColor titleColor "" . const mempty
 	-- Separator character
 		, ppSep =  "<fc=" ++ seperatorColor ++ "> <fn=1>|</fn> </fc>"
 	-- Urgent workspace
 		, ppUrgent = xmobarColor textColor "" . wrap "!" "!"  
 	-- Adding # of windows on current workspace to the 
 		, ppExtras  = [windowCount]
 	-- order of things in xmobar
  		, ppOrder  = \(ws:l:t:ex) -> [ws]++ex++[t]		
	}
  }  

toggleFullscreen :: X ()
toggleFullscreen =
  withWindowSet $ \ws ->
  withFocused $ \w -> do
        let fullRect = W.RationalRect 0 0 1 1
        let isFullFloat = w `M.lookup` W.floating ws == Just fullRect
        windows $ if isFullFloat then W.sink w else W.float w fullRect

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
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
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
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
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
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
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
