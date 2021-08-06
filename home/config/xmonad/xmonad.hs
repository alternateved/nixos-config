------------------------------------------------------------------------
-- IMPORTS
-------------------------------------------------------------------------
module Main (main) where

-- Data
import qualified Data.Map as M
import Data.Maybe (fromJust)
-- Base
import XMonad hiding ((|||))
-- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.DynamicProjects
import XMonad.Actions.GroupNavigation
  ( Direction (History),
    historyHook,
    nextMatch,
  )
import XMonad.Actions.Navigation2D
  ( Direction2D(L, R),
    windowGo,
    windowSwap,
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
import XMonad.Layout.LayoutCombinators ((|||), JumpToLayout(JumpToLayout))
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
    addTabs,
    shrinkText,
  )
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Simplest (Simplest (..))
import XMonad.Layout.SubLayouts (GroupMsg (UnMerge), mergeDir, onGroup, subLayout)
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
    wsWindows,
    windowMultiPrompt,
  )
import qualified XMonad.StackSet as W
-- Utilities
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
myFont = "xft:Iosevka Nerd Font Mono:style=regular:size=12:antialias=true:hinting=true"

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
myBorderWidth = 2

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
colorBg       = "#18181B"
colorFg       = "#E4E4E8"
colorHiWhite  = "#EFEFF1"
colorLoGrey   = "#4b5254"
colorHiGrey   = "#879193"
colorRed      = "#CD5C60"
colorBlue     = "#91B9C7"
colorGreen    = "#6FB593"


hiWhite, loWhite, loGrey, hiGrey, red :: String -> String
loWhite = xmobarColor colorFg ""
hiWhite = xmobarColor colorHiWhite ""
loGrey  = xmobarColor colorLoGrey ""
hiGrey  = xmobarColor colorHiGrey ""
red     = xmobarColor colorRed ""

-------------------------------------------------------------------------
-- STARTUPHOOK
-------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawnOnce "autorandr -c &"
  spawnOnce "xfce4-power-manager &"
  spawnOnce "bluetoothctl power on"
  spawnOnce "nitrogen --restore &"
  spawnOnce "emacs --daemon &"
  -- spawnOnce "firefox-devedition &"
  -- spawnOnce "signal-desktop &"
  -- spawnOnce "thunderbird &"
  setWMName "LG3D"

-------------------------------------------------------------------------
-- MANAGEHOOK
-------------------------------------------------------------------------
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Thunderbird" --> doShift (myWorkspaces !! 1)
    , className =? "Signal" --> doShift (myWorkspaces !! 1)
    , className =? "discord" --> doShift (myWorkspaces !! 1)
    , isFullscreen --> doFullFloat
    , isDialog --> doCenterFloat
    ] <+> namedScratchpadManageHook myScratchPads

-------------------------------------------------------------------------
-- LOGHOOK
-------------------------------------------------------------------------
myLogHook :: X ()
myLogHook = refocusLastLogHook
            <> historyHook
            <> updatePointer (0.5, 0.5) (0.5, 0.5)

-------------------------------------------------------------------------
-- WORKSPACES
-------------------------------------------------------------------------
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["Highway", "Communication", "Development", "System", "Other"]

-------------------------------------------------------------------------
-- PROJECTS
-------------------------------------------------------------------------
myProjects :: [Project]
myProjects =
  [ Project
      { projectName = "Highway"
      , projectDirectory = "~/"
      , projectStartHook = Nothing
      }
  , Project
      { projectName = "Communication"
      , projectDirectory = "~/"
      , projectStartHook = Nothing
      }
  , Project
      { projectName = "Development"
      , projectDirectory = "~/"
      , projectStartHook = Nothing
      }
  , Project
      { projectName = "System"
      , projectDirectory = "~/"
      , projectStartHook = Nothing
      }
  , Project
      { projectName = "Other"
      , projectDirectory = "~/"
      , projectStartHook = Nothing
      }
  ]

-------------------------------------------------------------------------
-- TABS CONFIGURATION
-------------------------------------------------------------------------
myTabConfig :: Theme
myTabConfig = def
    { fontName = myFont
    , activeTextColor = colorBg
    , activeColor = colorFg
    , activeBorderColor = colorFg
    , inactiveTextColor = colorFg
    , inactiveColor = colorBg
    , inactiveBorderColor = colorBg
    , urgentTextColor = colorBg
    , urgentColor = colorRed
    , urgentBorderColor = colorRed
    }

-------------------------------------------------------------------------
-- LAYOUTS
-------------------------------------------------------------------------
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw True (Border 0 i 0 i) True (Border i 0 i 0) True

monocle = renamed [Replace "Monocle"]
          $ noBorders
          $ Full

tall    = renamed [Replace "Tall"]
          $ addTabs shrinkText myTabConfig . subLayout [] Simplest
          $ avoidStruts
          $ mySpacing 5
          $ ResizableTall 1 (3 / 100) (1 / 2) []

wide    = renamed [Replace "Wide"]
          $ addTabs shrinkText myTabConfig . subLayout [] Simplest
          $ avoidStruts
          $ mySpacing 5
          $ Mirror
          $ ResizableTall 1 (3 / 100) (3 / 4) []

columns = renamed [Replace "Columns"]
          $ addTabs shrinkText myTabConfig . subLayout [] Simplest
          $ avoidStruts
          $ mySpacing 5
          $ ThreeColMid 1 (3 / 100) (12 / 30)

myLayoutHook = workspaceDir myHome
               $ smartBorders
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
               $ firstLayout . secondLayout . thirdLayout $ myDefaultLayout
             where
               firstLayout  = onWorkspace "Highway"       (monocle ||| tall ||| wide    ||| columns)
               secondLayout = onWorkspace "Communication" (tall    ||| wide ||| columns ||| monocle)
               thirdLayout  = onWorkspace "Development"   (columns ||| tall ||| wide    ||| monocle)
               myDefaultLayout =      tall
                                  ||| wide
                                  ||| columns
                                  ||| monocle

-------------------------------------------------------------------------
-- KEYBINDINGS
-------------------------------------------------------------------------
myKeys :: [(String, X ())]
myKeys =
  -- Xmonad
  [ ("M-S-r", spawn "xmonad --recompile; xmonad --restart")
  , ("M-C-r", spawn $ myEditor ++ xmonadConfig)
  , ("M-S-b", spawn $ "sh " ++ myDots ++ "/xmobar/xmobar_recompile.sh")
  , ("M-C-b", spawn $ myEditor ++ xmobarConfig)

    -- Open my preferred terminal
  , ("M-S-<Return>", spawn myTerminal)

    -- Run Prompt
  , ("M-p", shellPrompt myXPConfig)
  , ("M-S-p", myPrompt myTerminal myXPConfig)
  , ("M-S-q", dirExecPromptNamed myXPConfig' spawn (myDots ++ "/scripts/session") "Session: ")
  , ("M-S-d", changeDir myXPConfig')
  , ("M-'", windowMultiPrompt myXPConfig [(Goto, wsWindows), (Bring, allWindows)])

    -- Windows
  , ("M-S-c", kill1)
  , ("M-S-C-c", killAll)

    -- Floating windows
  , ("M-t", withFocused $ windows . W.sink)
  , ("M-S-t", sinkAll)

    -- Windows navigation
  , ("M-m", windows W.focusMaster)
  , ("M-j", windows W.focusDown)
  , ("M-k", windows W.focusUp)
  , ("M-h", windowGo L False)
  , ("M-l", windowGo R False)
  , ("M-S-m", windows W.swapMaster)
  , ("M-S-j", windows W.swapDown)
  , ("M-S-k", windows W.swapUp)
  , ("M-S-h", windowSwap L False)
  , ("M-S-l", windowSwap R False)
  , ("M-<Backspace>", promote)
  , ("M-S-<Tab>", rotSlavesDown)

    -- Urgent windows
  , ("M-g u", focusUrgent)
  , ("M-S-g u", clearUrgents)
  , ("M-g p", nextMatch History (return True))

    -- Layouts
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-C-h", sendMessage Shrink)
  , ("M-C-l", sendMessage Expand)
  , ("M-C-j", sendMessage MirrorShrink)
  , ("M-C-k", sendMessage MirrorExpand)
  , ("M-C-i", sendMessage (IncMasterN 1))
  , ("M-C-d", sendMessage (IncMasterN (-1)))


  , ("M-a t", sendMessage $ JumpToLayout "tall")
  , ("M-a w", sendMessage $ JumpToLayout "wide")
  , ("M-a c", sendMessage $ JumpToLayout "columns")
  , ("M-a m", sendMessage $ JumpToLayout "monocle")
  , ("M-f", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)

   -- SubLayouts
  , ("M-S-.", withFocused (sendMessage . mergeDir id))
  , ("M-S-,", withFocused (sendMessage . UnMerge))
  , ("M-.", onGroup W.focusUp')
  , ("M-,", onGroup W.focusDown')

    -- Scratchpads
  , ("M-s t", scratchTerm)
  , ("M-s c", scratchCalc)
  , ("M-s v", scratchMixer)
  , ("M-s m", scratchMonitor)

    -- Notifications
  , ("C-S-\\", spawn "dunstctl set-paused toggle")

    --- My Applications (Super+Alt+Key)
  , ("M-M1-e", spawn myEditor)
  , ("M-M1-f", spawn myFileManager)
  , ("M-M1-b", spawn myBrowser)

    -- Multimedia Keys
  , ("<XF86AudioMute>", spawn "amixer -q set Master toggle")
  , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 5%-")
  , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 5%+")
  , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
  , ("M-<Insert>", unGrab *> spawn "flameshot screen -p ~/Pictures/Screenshots")
  , ("M-S-<Insert>", unGrab *> spawn "flameshot gui")
  , ("M-C-<KP_Equal>", spawn "autorandr -c")
  ]
    -- Reorder physical screens and make focus follow moved window
    -- "M-w" -- focus screen marked as 1
    -- "M-e" -- focus screen marked as 0
    -- M-S-[screenKeybind] -- move and focus window on particulart screen
  ++
  [ (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
       | let shiftAndView i = W.view i . W.shift i,
         (key, scr) <- zip "we" [0, 1],
         (action, mask) <- [(W.view, ""), (shiftAndView, "S-")]
  ]

-------------------------------------------------------------------------
-- SCRATCHPADS
-------------------------------------------------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "terminal"   spawnTerm    findTerm    medium
  , NS "calculator" spawnCalc    findCalc    small
  , NS "volumectl"  spawnMixer   findMixer   small
  , NS "monitor"    spawnMonitor findMonitor medium
  ]
  where
    spawnTerm = myTerminal ++ " --title scratchpad"
    findTerm = title =? "scratchpad"

    spawnCalc = "qalculate-gtk"
    findCalc = className =? "Qalculate-gtk"

    spawnMixer = myTerminal ++ " --title pulsemixer -e pulsemixer"
    findMixer = title =? "pulsemixer"

    spawnMonitor = myTerminal ++ " --title htop -e htop"
    findMonitor = title =? "htop"

    small = customFloating $ W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)
    medium = customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)
    large = customFloating $ W.RationalRect (1 / 10) (1 / 10) (4 / 5) (4 / 5)

scratchTerm, scratchMixer, scratchCalc, scratchMonitor :: X ()
scratchTerm    = namedScratchpadAction myScratchPads "terminal"
scratchMixer   = namedScratchpadAction myScratchPads "volumectl"
scratchCalc    = namedScratchpadAction myScratchPads "calculator"
scratchMonitor = namedScratchpadAction myScratchPads "monitor"

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
myXPConfig = def
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
      maxComplRows = Just 3,
      changeModeKey = xK_Super_L
    }

myXPConfig' :: XPConfig
myXPConfig' = myXPConfig
    { autoComplete = Nothing
    }

-------------------------------------------------------------------------
-- XMOBAR CONFIGURATION
-------------------------------------------------------------------------
myXmobarPP :: PP
myXmobarPP = namedScratchpadFilterOutWorkspacePP $ def
      { ppCurrent = hiWhite,
        ppVisible = hiWhite,
        ppHidden = hiGrey,
        ppHiddenNoWindows = loGrey,
        ppUrgent = red,
        ppTitle = loWhite . shorten 60,
        ppSep = loWhite " | ",
        ppExtras = [],
        ppOrder = \(ws : l : t : _) -> [ws, l] ++ [t]
      }

-------------------------------------------------------------------------
-- MAIN CONFIG
-------------------------------------------------------------------------
myConfig = def
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
    } `additionalKeysP` myKeys

-------------------------------------------------------------------------
-- MAIN
-------------------------------------------------------------------------
main :: IO ()
main = xmonad
     . dynamicProjects myProjects
     . docks
     . ewmh
     . withUrgencyHook NoUrgencyHook
     =<< statusBar "./.config/xmobar/xmobar" myXmobarPP toggleStrutsKey myConfig
    where
      toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
      toggleStrutsKey XConfig {modMask = m} = (m, xK_b)
