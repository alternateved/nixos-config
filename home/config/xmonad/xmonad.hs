------------------------------------------------------------------------
-- IMPORTS
-------------------------------------------------------------------------
module Main (main) where

-- Data
import qualified Data.Map as M
import Data.Maybe (fromJust)
-- Base
import XMonad
  ( ChangeLayout (NextLayout),
    Default (def),
    Dimension,
    Full (Full),
    KeyMask,
    KeySym,
    Layout,
    ManageHook,
    Mirror (Mirror),
    Resize (Expand, Shrink),
    X,
    XConfig
      ( XConfig,
        borderWidth,
        focusedBorderColor,
        handleEventHook,
        layoutHook,
        logHook,
        manageHook,
        modMask,
        normalBorderColor,
        startupHook,
        terminal,
        workspaces
      ),
    className,
    composeAll,
    doShift,
    io,
    mod4Mask,
    screenWorkspace,
    sendMessage,
    spawn,
    title,
    whenJust,
    windows,
    withFocused,
    xC_left_ptr,
    xK_b,
    xmonad,
    (-->),
    (<+>),
    (=?),
    (|||),
  )
-- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.GroupNavigation
  ( Direction (History),
    historyHook,
    nextMatch,
  )
import XMonad.Actions.Promote (promote)
import XMonad.Actions.RotSlaves (rotSlavesDown)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.WithAll (killAll, sinkAll)
-- Hooks
import XMonad.Hooks.DynamicLog
  ( PP
      ( ppCurrent,
        ppExtras,
        ppHidden,
        ppHiddenNoWindows,
        ppOrder,
        ppSep,
        ppTitle,
        ppUrgent,
        ppVisible
      ),
    shorten,
    statusBar,
    xmobarColor,
  )
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.InsertPosition
  ( Focus (Newer),
    Position (Master),
    insertPosition,
  )
import XMonad.Hooks.ManageDocks (avoidStruts, docks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, isDialog, isFullscreen)
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.UrgencyHook
  ( NoUrgencyHook (NoUrgencyHook),
    clearUrgents,
    focusUrgent,
    withUrgencyHook,
  )
-- Layouts
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.MultiToggle (mkToggle, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, NOBORDERS))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.ResizableTile
  ( MirrorResize (..),
    ResizableTall (ResizableTall),
  )
import XMonad.Layout.Spacing
  ( Border (Border),
    Spacing,
    spacingRaw,
  )
import XMonad.Layout.Tabbed
  ( Theme (..),
    shrinkText,
    tabbed,
  )
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Layout.WorkspaceDir (changeDir, workspaceDir)
-- Prompt
import XMonad.Prompt
  ( XPConfig (..),
    XPPosition (Top),
    XPrompt,
    completionToCommand,
    mkXPrompt,
    showXPrompt,
  )
import XMonad.Prompt.DirExec (dirExecPromptNamed)
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)
import XMonad.Prompt.Shell
  ( getCommands,
    getShellCompl,
    shellPrompt,
  )
import XMonad.Prompt.Window
  ( WindowPrompt (Bring, Goto),
    allWindows,
    windowPrompt,
  )
import qualified XMonad.StackSet as W
-- Utilities
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad (NS),
    customFloating,
    namedScratchpadAction,
    namedScratchpadFilterOutWorkspacePP,
    namedScratchpadManageHook,
  )
import XMonad.Util.Run (unsafeSpawn)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Ungrab (unGrab)

-------------------------------------------------------------------------
-- VARIABLES
-------------------------------------------------------------------------
myHome :: String
myHome = "/home/alternateved"

myDots :: String
myDots = myHome ++ "/.nixos-config/home/config"

xmonadConfig :: String
xmonadConfig = myDots ++ "/xmonad/xmonad.hs"

xmobarConfig :: String
xmobarConfig = myDots ++ "/xmobar/xmobar.hs"

myFont :: String
myFont = "xft:JetBrainsMono Nerd Font:style=medium:size=10:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox-devedition"

myFileManager :: String
myFileManager = myTerminal ++ " -e ranger"

myEditor :: String
myEditor = "emacsclient -a '' -c "

myBorderWidth :: Dimension
myBorderWidth = 1

-------------------------------------------------------------------------
-- COLORS
-------------------------------------------------------------------------
-- Window colors
myNormColor :: String
myNormColor = colorBg

myFocusColor :: String
myFocusColor = colorFg

-- Base colors
colorBg, colorFg, colorHiWhite, colorLoGrey, colorHiGrey, colorRed, colorBlue, colorGreen :: String
colorBg = "#1d1f21"
colorFg = "#c4c8c5"
colorHiWhite = "#ecf0ed"
colorLoGrey = "#545B68"
colorHiGrey = "#9fa1a3"
colorRed = "#cc6666"
colorBlue = "#80a1bd"
colorGreen = "#b5bd68"

hiWhite, loWhite, loGrey, hiGrey, red :: String -> String
loWhite = xmobarColor colorFg ""
hiWhite = xmobarColor colorHiWhite ""
loGrey = xmobarColor colorLoGrey ""
hiGrey = xmobarColor colorHiGrey ""
red = xmobarColor colorRed ""

-------------------------------------------------------------------------
-- STARTUPHOOK
-------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
  setDefaultCursor xC_left_ptr
  -- spawnOnce "autorandr -c &"
  spawnOnce "lxpolkit &"
  spawnOnce "xfce4-power-manager &"
  spawnOnce "dunst &"
  spawnOnce "bluetoothctl power on"
  spawnOnce "nitrogen --restore &"
  spawnOnce "emacs --daemon &"
  spawnOnce "firefox-devedition &"
  spawnOnce "signal-desktop &"
  spawnOnce "thunderbird &"
  setWMName "LG3D"

-------------------------------------------------------------------------
-- MANAGEHOOK
-------------------------------------------------------------------------
myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Firefox Developer Edition" --> doShift (head myWorkspaces),
      className =? "Thunderbird" --> doShift (myWorkspaces !! 1),
      className =? "Signal" --> doShift (myWorkspaces !! 1),
      className =? "discord" --> doShift (myWorkspaces !! 1),
      isFullscreen --> doFullFloat,
      isDialog --> doCenterFloat,
      insertPosition Master Newer
    ]
    <+> namedScratchpadManageHook myScratchPads

-------------------------------------------------------------------------
-- LOGHOOK
-------------------------------------------------------------------------
myLogHook :: X ()
myLogHook =
  refocusLastLogHook
    <> historyHook
    <> updatePointer (0.5, 0.5) (0.5, 0.5)

-------------------------------------------------------------------------
-- WORKSPACES
-------------------------------------------------------------------------
myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myWorkspaceIndices :: M.Map String Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1 ..] -- (,) == \x y -> (x,y)

clickable :: [Char] -> [Char]
clickable ws = "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
  where
    i = fromJust $ M.lookup ws myWorkspaceIndices

-------------------------------------------------------------------------
-- TABS CONFIGURATION
-------------------------------------------------------------------------
myTabConfig :: Theme
myTabConfig =
  def
    { fontName = myFont,
      activeTextColor = colorHiWhite,
      activeColor = colorBg,
      activeBorderColor = colorFg,
      inactiveTextColor = colorHiGrey,
      inactiveColor = colorBg,
      inactiveBorderColor = colorBg,
      urgentTextColor = colorRed,
      urgentColor = colorBg,
      urgentBorderColor = colorBg,
      decoHeight = 20
    }

-------------------------------------------------------------------------
-- LAYOUTS
-------------------------------------------------------------------------
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i 0 i 0) True (Border 0 i 0 i) True

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw False (Border i i i i) True (Border 0 0 0 0) True

tall =
  renamed [Replace "tall"] $
    avoidStruts $
      mySpacing 5 $
        ResizableTall 1 (3 / 100) (1 / 2) []

wide =
  renamed [Replace "wide"] $
    avoidStruts $
      mySpacing 5 $
        Mirror $
          ResizableTall 1 (3 / 100) (3 / 4) []

columns =
  renamed [Replace "columns"] $
    avoidStruts $
      mySpacing 5 $
        ThreeColMid 1 (3 / 100) (12 / 30)

tabs =
  renamed [Replace "tabs"] $
    avoidStruts $
      mySpacing' 5 $
        noBorders $
          tabbed shrinkText myTabConfig

myLayoutHook =
  workspaceDir myHome $
    smartBorders $
      mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout =
      tall
        ||| wide
        ||| columns
        ||| tabs

-------------------------------------------------------------------------
-- KEYBINDINGS
-------------------------------------------------------------------------
myKeys :: [(String, X ())]
myKeys =
  -- Xmonad
  [ ("M-S-r", spawn "xmonad --recompile; xmonad --restart"), -- Recompile and restart xmonad
    ("M-C-r", spawn $ myEditor ++ xmonadConfig), -- Modify configuration file
    ("M-S-b", spawn $ "sh " ++ myDots ++ "/xmobar/xmobar_recompile.sh"), -- Modify configuration file
    ("M-C-b", spawn $ myEditor ++ xmobarConfig), -- Modify configuration file

    -- Open my preferred terminal
    ("M-S-<Return>", spawn myTerminal),

    -- Run Prompt
    ("M-p", shellPrompt myXPConfig),
    ("M-S-p", myPrompt myTerminal myXPConfig),
    ("M-S-q", dirExecPromptNamed myXPConfig' spawn (myDots ++ "/scripts/session") "Session: "),
    ("M-S-d", changeDir myXPConfig'),
    ("M-d b", windowPrompt myXPConfig Bring allWindows),
    ("M-d g", windowPrompt myXPConfig Goto allWindows),

    -- Windows
    ("M-S-c", kill1), -- Kill the currently focused client
    ("M-S-a", killAll), -- Kill all windows on current workspace

    -- Floating windows
    ("M-t", withFocused $ windows . W.sink), -- Push floating window back to tile
    ("M-S-t", sinkAll), -- Push ALL floating windows to tile

    -- Windows navigation
    ("M-m", windows W.focusMaster), -- Move focus to the master window
    ("M-j", windows W.focusDown), -- Move focus to the next window
    ("M-k", windows W.focusUp), -- Move focus to the prev window
    ("M-S-m", windows W.swapMaster), -- Swap the focused window and the master window
    ("M-S-j", windows W.swapDown), -- Swap focused window with next window
    ("M-S-k", windows W.swapUp), -- Swap focused window with prev window
    ("M-<Backspace>", promote), -- Moves focused window to master, others maintain order
    ("M-g u", focusUrgent), -- Go to urgent window
    ("M-S-g u", clearUrgents), -- Clear all urgent windows
    ("M-<Tab>", rotSlavesDown),  -- Rotate all windows except master and keep focus in place
    ("M-g p", nextMatch History (return True)), -- Go to previous window

    -- Layouts
    ("M-<Space>", sendMessage NextLayout), -- Switch to next layout
    ("M-C-h", sendMessage Shrink), -- Shrink horiz window width
    ("M-C-l", sendMessage Expand), -- Expand horiz window width
    ("M-C-j", sendMessage MirrorShrink), -- Shrink vert window width
    ("M-C-k", sendMessage MirrorExpand), -- Expand vert window width
    ("M-a m", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts), -- Toggles noborder/full

    -- Scratchpads
    ("M-s t", scratchTerm),
    ("M-s c", scratchCalc),
    ("M-s v", scratchMixer),

    -- Notifications
    ("C-M1-\\", spawn "dunstctl set-paused toggle"), -- Toggle dunst notifications

    --- My Applications (Super+Alt+Key)
    ("M-M1-e", spawn myEditor),
    ("M-M1-f", spawn myFileManager),
    ("M-M1-b", spawn myBrowser),

    -- Multimedia Keys
    ("<XF86AudioMute>", spawn "amixer -q set Master toggle"),
    ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 5%-"),
    ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 5%+"),
    ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10"),
    ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10"),
    ("M-<Insert>", unGrab *> spawn "flameshot screen -p ~/Pictures/Screenshots"),
    ("M-S-<Insert>", unGrab *> spawn "flameshot gui")
  ]
    -- Reorder physical screens and make focus follow moved window
    -- "M-w" -- focus screen marked as 1
    -- "M-e" -- focus screen marked as 0
    -- M-S-[screenKeybind] -- move and focus window on particulart screen
    ++ [ (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
         | let shiftAndView i = W.view i . W.shift i,
           (key, scr) <- zip "we" [0, 1],
           (action, mask) <- [(W.view, ""), (shiftAndView, "S-")]
       ]

-------------------------------------------------------------------------
-- SCRATCHPADS
-------------------------------------------------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "terminal" spawnTerm findTerm medium,
    NS "calculator" spawnCalc findCalc small,
    NS "volumectl" spawnMixer findMixer small
  ]
  where
    spawnTerm = myTerminal ++ " --title scratchpad"
    findTerm = title =? "scratchpad"

    spawnCalc = "qalculate-gtk"
    findCalc = className =? "Qalculate-gtk"

    spawnMixer = myTerminal ++ " --title pulsemixer -e pulsemixer"
    findMixer = title =? "pulsemixer"

    small = customFloating $ W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)
    medium = customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)
    large = customFloating $ W.RationalRect (1 / 10) (1 / 10) (4 / 5) (4 / 5)

scratchTerm, scratchMixer, scratchCalc :: X ()
scratchTerm = namedScratchpadAction myScratchPads "terminal"
scratchMixer = namedScratchpadAction myScratchPads "volumectl"
scratchCalc = namedScratchpadAction myScratchPads "calculator"

-------------------------------------------------------------------------
-- PROMPT
-------------------------------------------------------------------------
data TShell = TShell

instance XPrompt TShell where
  showXPrompt TShell = "Run in terminal: "
  completionToCommand _ = escape
    where
      escape (x : xs)
        | isSpecialChar x = '\\' : x : escape xs
        | otherwise = x : escape xs
      isSpecialChar = flip elem " &\\@\"'#?$*()[]{};"

myPrompt :: FilePath -> XPConfig -> X ()
myPrompt c config = do
  cmds <- io getCommands
  mkXPrompt TShell config (getShellCompl cmds $ searchPredicate config) run
  where
    run a = unsafeSpawn $ c ++ " -e " ++ a

-------------------------------------------------------------------------
-- PROMPT CONFIGURATION
-------------------------------------------------------------------------
myXPConfig :: XPConfig
myXPConfig =
  def
    { font = myFont,
      bgColor = colorBg,
      fgColor = colorFg,
      bgHLight = colorFg,
      fgHLight = colorBg,
      borderColor = colorBg,
      promptBorderWidth = 0,
      position = Top,
      height = 18,
      historySize = 256,
      historyFilter = id,
      defaultText = [],
      autoComplete = Just 100000,
      showCompletionOnTab = False,
      searchPredicate = fuzzyMatch,
      alwaysHighlight = True,
      maxComplRows = Nothing
    }

myXPConfig' :: XPConfig
myXPConfig' =
  myXPConfig
    { autoComplete = Nothing
    }

-------------------------------------------------------------------------
-- XMOBAR CONFIGURATION
-------------------------------------------------------------------------
myXmobarPP :: PP
myXmobarPP =
  namedScratchpadFilterOutWorkspacePP $
    def
      { ppCurrent = hiWhite,
        ppVisible = hiWhite . clickable,
        ppHidden = hiGrey . clickable,
        ppHiddenNoWindows = loGrey . clickable,
        ppUrgent = red . clickable,
        ppTitle = loWhite . shorten 60,
        ppSep = loWhite " | ",
        ppExtras = [],
        ppOrder = \(ws : l : t : _) -> [ws, l] ++ [t]
      }

-------------------------------------------------------------------------
-- MAIN CONFIG
-------------------------------------------------------------------------
myConfig =
  def
    { manageHook = myManageHook,
      modMask = myModMask,
      terminal = myTerminal,
      startupHook = myStartupHook,
      logHook = myLogHook,
      layoutHook = myLayoutHook,
      handleEventHook = fullscreenEventHook,
      workspaces = myWorkspaces,
      borderWidth = myBorderWidth,
      normalBorderColor = myNormColor,
      focusedBorderColor = myFocusColor
    }
    `additionalKeysP` myKeys

-------------------------------------------------------------------------
-- MAIN
-------------------------------------------------------------------------
main :: IO ()
main =
  xmonad
    . docks
    . ewmh
    . withUrgencyHook NoUrgencyHook
    =<< statusBar "./.config/xmobar/xmobar" myXmobarPP toggleStrutsKey myConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig {modMask = m} = (m, xK_b)
