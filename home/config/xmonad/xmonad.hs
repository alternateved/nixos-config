------------------------------------------------------------------------
-- IMPORTS
-------------------------------------------------------------------------
module Main (main) where

-- Data
import Data.Bifunctor (bimap)
import Data.Char (isSpace)
import Data.Monoid (All(..))
import Data.List (dropWhileEnd, elemIndex, find)
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import qualified Data.Map as M
-- System
import System.IO.Unsafe (unsafeDupablePerformIO)
-- Base
import XMonad hiding ((|||))
-- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, renameWorkspace, removeWorkspace, withNthWorkspace)
import XMonad.Actions.GroupNavigation (Direction (History), historyHook, nextMatch,)
import XMonad.Actions.Navigation2D (Direction2D(U, D, L, R), windowGo, windowSwap)
import XMonad.Actions.Promote (promote)
import XMonad.Actions.RotSlaves (rotSlavesDown)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.WithAll (killAll, sinkAll)
-- Hooks
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.InsertPosition (Focus (Newer), Position (Below), insertPosition)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, isDialog)
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.StatusBar (StatusBarConfig, dynamicSBs, statusBarPropTo)
import XMonad.Hooks.StatusBar.PP hiding (trim)
import XMonad.Hooks.UrgencyHook (NoUrgencyHook (NoUrgencyHook), clearUrgents, focusUrgent, withUrgencyHook)
-- Layouts
import XMonad.Layout.LayoutCombinators ((|||), JumpToLayout (JumpToLayout))
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.MultiToggle (mkToggle, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL, NOBORDERS))
import qualified XMonad.Layout.MultiToggle as MT (Toggle (..))
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.ResizableTile (MirrorResize (..), ResizableTall (ResizableTall))
import XMonad.Layout.Spacing (Border (Border), Spacing, spacingRaw)
import XMonad.Layout.Tabbed (Theme (..), addTabs, shrinkText)
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Simplest (Simplest (..))
import XMonad.Layout.SubLayouts (GroupMsg (UnMerge), mergeDir, onGroup, subLayout)
import XMonad.Layout.WorkspaceDir (changeDir, workspaceDir)
import XMonad.Operations
-- Prompt
import XMonad.Prompt (XPConfig (..), XPPosition (Top), XPrompt, completionToCommand, mkXPrompt, showXPrompt, vimLikeXPKeymap)
import XMonad.Prompt.DirExec (dirExecPromptNamed)
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)
import XMonad.Prompt.Shell (getCommands, getShellCompl, shellPrompt)
import XMonad.Prompt.Window (WindowPrompt (Bring, Goto), windowPrompt, allWindows)
import XMonad.Prompt.XMonad (xmonadPrompt)
import qualified XMonad.StackSet as W
-- Utilities
import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers (Logger, logCurrentOnScreen, logLayoutOnScreen, logTitleOnScreen, shortenL, xmobarColorL)
import XMonad.Util.NamedScratchpad (NamedScratchpad (NS), customFloating, namedScratchpadAction, namedScratchpadFilterOutWorkspacePP, namedScratchpadManageHook)
import XMonad.Util.Run (runProcessWithInput, unsafeSpawn)
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
xmobarConfig = myDots ++ "/xmonad/xmobar.hs"

myFont :: String
myFont = "xft:Iosevka Nerd Font Mono:style=regular:size=12:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "qutebrowser"

myFileManager :: String
myFileManager = myTerminal ++ " -e vifm"

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
colorBg       = basebg
colorFg       = basefg
colorHiWhite  = base15
colorLoGrey   = base00
colorHiGrey   = base08
colorRed      = base01
colorBlue     = base04
colorGreen    = base02

hiWhite, loWhite, loGrey, hiGrey, red :: String -> String
loWhite = xmobarColor colorFg ""
hiWhite = xmobarColor colorHiWhite ""
loGrey  = xmobarColor colorLoGrey ""
hiGrey  = xmobarColor colorHiGrey ""
red     = xmobarColor colorRed ""

hiWhiteL :: Logger -> Logger
hiWhiteL     = xmobarColorL colorHiWhite ""

-------------------------------------------------------------------------
-- STARTUPHOOK
-------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = setWMName "LG3D"

-------------------------------------------------------------------------
-- MANAGEHOOK
-------------------------------------------------------------------------
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Thunderbird" --> doShift (myWorkspaces !! 2)
    , className =? "Signal" --> doShift (myWorkspaces !! 2)
    , className =? "discord" --> doShift (myWorkspaces !! 2)
    , className =? "mpv" --> doShift (myWorkspaces !! 5)
    , className =? "Firefox Developer Edition" --> doShift (myWorkspaces !! 5)
    , isDialog --> doCenterFloat
    , insertPosition Below Newer
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
myWorkspaces = ["Highway", "Communication", "Development", "System", "Media", "Other"]

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

monocle = renamed [Replace "monocle"]
          $ avoidStruts
          $ noBorders
          $ Full

tall    = renamed [Replace "tall"]
          $ addTabs shrinkText myTabConfig . subLayout [] Simplest
          $ avoidStruts
          $ mySpacing 5
          $ ResizableTall 1 (3 / 100) (1 / 2) []

wide    = renamed [Replace "wide"]
          $ addTabs shrinkText myTabConfig . subLayout [] Simplest
          $ avoidStruts
          $ mySpacing 5
          $ Mirror
          $ ResizableTall 1 (3 / 100) (3 / 4) []

columns = renamed [Replace "columns"]
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
  [ ("M-S-r", spawn "xmonad --restart")
  , ("M-C-r", spawn $ myEditor ++ xmonadConfig)
  , ("M-S-b", spawn $ "sh " ++ myDots ++ "/xmonad/xmobar_recompile.sh")
  , ("M-C-b", spawn $ myEditor ++ xmobarConfig)

    -- Open my preferred terminal
  , ("M-S-<Return>", spawn myTerminal)

    -- Prompts
  , ("M-p", shellPrompt myXPConfig)
  , ("M-S-p", myPrompt myTerminal myXPConfig)
  , ("M-S-q", dirExecPromptNamed myXPConfig' spawn (myDots ++ "/scripts/session") "Session: ")
  , ("M-'", windowPrompt myXPConfig Bring allWindows)
  , ("M-S-'", windowPrompt myXPConfig Goto allWindows)
  , ("M-S-;", xmonadPrompt myXPConfig)

  -- Workspace management
  , ("M-y a", addWorkspacePrompt myXPConfig')
  , ("M-y r", renameWorkspace myXPConfig')
  , ("M-y c", changeDir myXPConfig')
  , ("M-y d", removeWorkspace)

    -- Windows
  , ("M-S-c", kill1)
  , ("M-C-c", killAll)

    -- Floating windows
  , ("M-t", withFocused $ windows . W.sink)
  , ("M-S-t", sinkAll)

    -- Windows navigation
  , ("M-m", windows W.focusMaster)
  , ("M-j", windowGo D False)
  , ("M-k", windowGo U False)
  , ("M-h", windowGo L False)
  , ("M-l", windowGo R False)
  , ("M-S-m", windows W.swapMaster)
  , ("M-S-j", windowSwap D False)
  , ("M-S-k", windowSwap U False)
  , ("M-S-h", windowSwap L False)
  , ("M-S-l", windowSwap R False)
  , ("M-n", windows W.focusDown)
  , ("M-S-n", windows W.focusUp)
  , ("M-<Backspace>", promote)
  , ("M-S-<Tab>", rotSlavesDown)

    -- Urgent windows
  , ("M-u", focusUrgent)
  , ("M-S-u", clearUrgents)
  , ("M-C-u", nextMatch History (return True))

    -- Layouts
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-C-h", sendMessage Shrink)
  , ("M-C-l", sendMessage Expand)
  , ("M-C-j", sendMessage MirrorShrink)
  , ("M-C-k", sendMessage MirrorExpand)
  , ("M-i", sendMessage (IncMasterN 1))
  , ("M-d", sendMessage (IncMasterN (-1)))
  , ("M-b", sendMessage ToggleStruts)

  , ("M-a m", sendMessage $ JumpToLayout "monocle")
  , ("M-a t", sendMessage $ JumpToLayout "tall")
  , ("M-a w", sendMessage $ JumpToLayout "wide")
  , ("M-a c", sendMessage $ JumpToLayout "columns")
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
  , ("M-C-<KP_Equal>", spawn "autorandr -cf")
  ]
  ++ workspaceKeys
  ++ screenKeys
 where
  workspaceNumbers = [1 :: Int .. 9] <> [0]
  workspaceKeys =
    [ ("M-" <> m <> show k, withNthWorkspace f i)
    | (k, i) <- zip workspaceNumbers [0 ..]
    , (m, f) <- [("", W.view), ("C-", W.greedyView), ("S-", W.shift)]
    ]
  screenKeys =
    [ ("M-" <> m <> show k, screenWorkspace s >>= flip whenJust (windows . f))
    | (k, s) <- zip "we" [0 ..]
    , (m, f) <- zip ["", "S-"] [W.view, W.shift]
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
    { font = myFont
    , bgColor = colorBg
    , fgColor = colorFg
    , bgHLight = colorFg
    , fgHLight = colorBg
    , borderColor = colorBg
    , promptBorderWidth = 0
    , position = Top
    , height = 18
    , historySize = 100
    , historyFilter = id
    , defaultText = []
    , autoComplete = Just 100000
    , showCompletionOnTab = False
    , promptKeymap = vimLikeXPKeymap
    , searchPredicate = fuzzyMatch
    , alwaysHighlight = True
    , maxComplRows = Just 3
    , changeModeKey = xK_Super_L
    }

myXPConfig' :: XPConfig
myXPConfig' = myXPConfig
    { autoComplete = Nothing
    }

-------------------------------------------------------------------------
-- XMOBAR CONFIGURATION
-------------------------------------------------------------------------
mainXmobarPP :: ScreenId -> X PP
mainXmobarPP s = clickablePP . namedScratchpadFilterOutWorkspacePP $ def
      { ppCurrent = hiWhite . xmobarBorder "Bottom" myFocusColor 1
      , ppVisible = hiWhite
      , ppHidden = hiGrey
      , ppHiddenNoWindows = loGrey
      , ppUrgent = red
      , ppTitle = loWhite . shorten 60
      , ppSep = loWhite " | "
      , ppExtras  = [ logLayoutOnScreen s
                    , shortenL 70 $ logTitleOnScreen s
                    ]
      , ppOrder = \(ws : _ : _ : extras) -> ws : extras
      }

auxXmobarPP :: ScreenId -> X PP
auxXmobarPP s = pure $ def
    { ppOrder  = \(_ : _ : _ : extras) -> extras
    , ppSep = loWhite " | "
    , ppExtras = [ hiWhiteL $ logCurrentOnScreen s
                 , logLayoutOnScreen s
                 , shortenL 70 $ logTitleOnScreen s
                 ]
    }

-------------------------------------------------------------------------
-- XMOBAR INSTANCES
-------------------------------------------------------------------------
xmobarExec :: String
xmobarExec = myHome ++ "/.config/xmobar/xmobar"

xmobar0, xmobar1 :: StatusBarConfig
xmobar0 = statusBarPropTo "xmobar0" (xmobarExec)           (mainXmobarPP 0)
xmobar1 = statusBarPropTo "xmobar1" (xmobarExec ++ " aux") (auxXmobarPP  1)

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner 0 = pure xmobar0
barSpawner 1 = pure xmobar1
barSpawner _ = mempty

-------------------------------------------------------------------------
-- HELPER FUNCTIONS
-------------------------------------------------------------------------
xProperty :: String -> IO String
xProperty key = fromMaybe "" . findValue key <$> runProcessWithInput "xrdb" ["-query"] ""

findValue :: String -> String -> Maybe String
findValue xresKey xres = snd <$> find ((== xresKey) . fst) (catMaybes $ splitAtColon <$> lines xres)

splitAtColon :: String -> Maybe (String, String)
splitAtColon str = splitAtTrimming str <$> elemIndex ':' str

splitAtTrimming :: String -> Int -> (String, String)
splitAtTrimming str idx = bimap trim (trim . tail) $ splitAt idx str

trim, xprop :: ShowS
trim = dropWhileEnd isSpace . dropWhile isSpace
xprop = unsafeDupablePerformIO . xProperty

basebg, basefg, base00, base08, base01, base02, base04, base15 :: String
basebg = xprop "*.background"
basefg = xprop "*.foreground"
base00 = xprop "*.color0"
base08 = xprop "*.color8"
base01 = xprop "*.color1"
base02 = xprop "*.color2"
base04 = xprop "*.color4"
base15 = xprop "*.color15"


-------------------------------------------------------------------------
-- NIXOS RESTART HOOK
-------------------------------------------------------------------------
restartEventHook e@ClientMessageEvent { ev_message_type = mt } = do
  a <- getAtom "XMONAD_RESTART"
  if mt == a
    then XMonad.Operations.restart "alternateved-xmonad" True >> return (All True)
    else return $ All True
restartEventHook _ = return $ All True

-------------------------------------------------------------------------
-- MAIN CONFIG
-------------------------------------------------------------------------
myConfig = def
    { manageHook = myManageHook
    , modMask = myModMask
    , terminal = myTerminal
    , startupHook = myStartupHook
    , logHook = myLogHook
    , layoutHook = myLayoutHook
    , handleEventHook = restartEventHook
    , workspaces = myWorkspaces
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormColor
    , focusedBorderColor = myFocusColor
    } `additionalKeysP` myKeys

-------------------------------------------------------------------------
-- MAIN
-------------------------------------------------------------------------
main :: IO ()
main = xmonad
     . docks
     . ewmh
     . ewmhFullscreen
     . withUrgencyHook NoUrgencyHook
     . dynamicSBs barSpawner
     $ myConfig
