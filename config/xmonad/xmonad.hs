------------------------------------------------------------------------
-- IMPORTS
-------------------------------------------------------------------------
module Main (main) where

-- Data
import Data.Bifunctor (bimap)
import Data.Char (isSpace)
import Data.Monoid (All(..))
import Data.List (dropWhileEnd, elemIndex, find)
import Data.Maybe (catMaybes, isJust, fromJust, fromMaybe)
import qualified Data.Map as M
-- System
import System.IO.Unsafe (unsafeDupablePerformIO)
-- Base
import XMonad
-- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (prevWS, nextWS)
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, renameWorkspace, removeWorkspace, withNthWorkspace)
import XMonad.Actions.GroupNavigation (Direction (History), historyHook, nextMatch,)
import XMonad.Actions.Navigation2D (Direction2D (..), defaultTiledNavigation, centerNavigation, layoutNavigation, sideNavigation, singleWindowRect, unmappedWindowRect, windowGo, windowSwap, withNavigation2DConfig)
import XMonad.Actions.Promote (promote)
import qualified XMonad.Actions.Search as S (SearchEngine (..), promptSearch, selectSearch, searchEngine, searchEngineF)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.WithAll (killAll, sinkAll)
-- Hooks
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.InsertPosition (Focus (Newer), Position (Below), insertPosition)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, ToggleStruts (..))
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, isDialog)
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import XMonad.Hooks.Rescreen (addRandrChangeHook)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.StatusBar (StatusBarConfig, dynamicSBs, statusBarPropTo)
import XMonad.Hooks.StatusBar.PP hiding (trim)
import XMonad.Hooks.UrgencyHook (NoUrgencyHook (NoUrgencyHook), clearUrgents, focusUrgent, withUrgencyHook)
-- Layouts
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.BorderResize (borderResize)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL, NOBORDERS))
import qualified XMonad.Layout.MultiToggle as MT (Toggle (..))
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Reflect (REFLECTX (..))
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.ResizableTile (MirrorResize (..), ResizableTall (ResizableTall))
import XMonad.Layout.Spacing (Border (Border), Spacing, spacingRaw)
import XMonad.Layout.Tabbed (Theme (..), addTabs, shrinkText)
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Layout.Simplest (Simplest (..))
import XMonad.Layout.SubLayouts (GroupMsg (UnMerge), mergeDir, onGroup, subLayout)
import XMonad.Layout.WorkspaceDir (changeDir, workspaceDir)
import XMonad.Operations
-- Prompt
import XMonad.Prompt (XPConfig (..), XPPosition (CenteredAt), emacsLikeXPKeymap)
import XMonad.Prompt.DirExec (dirExecPromptNamed)
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)
import XMonad.Prompt.Man (manPrompt)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Window (WindowPrompt (Bring, Goto), windowPrompt, allWindows, wsWindows)
import qualified XMonad.StackSet as W
-- Utilities
import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers (Logger, logCurrentOnScreen, logLayoutOnScreen, logTitleOnScreen, shortenL, xmobarColorL)
import XMonad.Util.NamedScratchpad (NamedScratchpad (NS), customFloating, namedScratchpadAction, namedScratchpadFilterOutWorkspacePP, namedScratchpadManageHook)
import XMonad.Util.Run (runProcessWithInput)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Ungrab (unGrab)

-------------------------------------------------------------------------
-- VARIABLES
-------------------------------------------------------------------------
myHome :: String
myHome = "/home/alternateved"

myDots :: String
myDots = myHome ++ "/.nixos-config/config"

xmonadConfig :: String
xmonadConfig = myDots ++ "/xmonad/xmonad.hs"

xmobarConfig :: String
xmobarConfig = myDots ++ "/xmobar/xmobar.hs"

myFont :: String
myFont = "xft:Iosevka Nerd Font Mono:style=regular:size=11:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox"

myFileManager :: String
myFileManager = myTerminal ++ " -e nnn"

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
colorBg, colorFg, colorHiWhite, colorBlack, colorHiBlack, colorRed, colorBlue, colorGreen :: String
colorBg       = basebg
colorFg       = basefg
colorWhite    = base07
colorHiWhite  = base15
colorBlack    = base00
colorHiBlack  = base08
colorRed      = base01
colorBlue     = base04
colorGreen    = base02

white, hiWhite, foreground, black, hiBlack, red :: String -> String
foreground = xmobarColor colorFg ""
white      = xmobarColor colorWhite ""
hiWhite    = xmobarColor colorHiWhite ""
black      = xmobarColor colorBlack ""
hiBlack    = xmobarColor colorHiBlack ""
red        = xmobarColor colorRed ""

foregroundL :: Logger -> Logger
foregroundL = xmobarColorL colorFg ""

-------------------------------------------------------------------------
-- STARTUPHOOK
-------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawnOnce "xargs xwallpaper --stretch < ~/.cache/wall"
  setWMName "LG3D"

-------------------------------------------------------------------------
-- MANAGEHOOK
-------------------------------------------------------------------------
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Firefox" --> doShift (myWorkspaces !! 0)
    , className =? "Thunderbird" --> doShift (myWorkspaces !! 1)
    , className =? "Signal" --> doShift (myWorkspaces !! 1)
    , className =? "discord" --> doShift (myWorkspaces !! 1)
    , className =? "mpv" --> doShift (myWorkspaces !! 4)
    , className =? "Peek" --> doCenterFloat
    , className =? "Sxiv" --> doCenterFloat
    , isDialog --> doCenterFloat
    , fmap not willFloat --> insertPosition Below Newer
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
    , activeColor = colorHiBlack
    , activeBorderColor = colorHiBlack
    , inactiveTextColor = colorHiBlack
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

tall    = renamed [Replace "tall"]
          $ addTabs shrinkText myTabConfig . subLayout [] Simplest
          $ avoidStruts
          $ mySpacing 5
          $ ResizableTall 1 (3 / 100) (1 / 2) []

columns = renamed [Replace "columns"]
          $ addTabs shrinkText myTabConfig . subLayout [] Simplest
          $ avoidStruts
          $ mySpacing 5
          $ ThreeColMid 1 (3 / 100) (12 / 30)

bsp     = renamed [Replace "bsp"]
          $ borderResize
          $ addTabs shrinkText myTabConfig . subLayout [] Simplest
          $ avoidStruts
          $ mySpacing 5
          $ emptyBSP

monocle = renamed [Replace "monocle"]
          $ addTabs shrinkText myTabConfig . subLayout [] Simplest
          $ avoidStruts
          $ Full

myLayoutHook = workspaceDir myHome
               $ smartBorders
               $ mkToggle (single REFLECTX)
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
               $ myDefaultLayout
            where
               myDefaultLayout =      tall
                                  ||| columns
                                  ||| bsp
                                  ||| monocle

-------------------------------------------------------------------------
-- KEYBINDINGS
-------------------------------------------------------------------------
myKeys :: [(String, X ())]
myKeys =
  -- Xmonad
  [ ("M-S-r", spawn "xmonad --restart")
  , ("M-C-r", spawn $ myEditor ++ xmonadConfig)
  , ("M-C-b", spawn $ myEditor ++ xmobarConfig)

    -- Open my preferred terminal
  , ("M-S-<Return>", spawn myTerminal)

    -- Prompts
  , ("M-p", shellPrompt myXPConfig)
  , ("M-S-q", dirExecPromptNamed myXPConfig' spawn (myDots ++ "/xmonad/scripts") "Session: ")
  , ("M-<F1>", manPrompt myXPConfig)
  , ("M-'", windowPrompt myXPConfig' Goto wsWindows)
  , ("M-C-'", windowPrompt myXPConfig' Goto allWindows)
  , ("M-S-'", windowPrompt myXPConfig' Bring allWindows)

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
  , ("M-j", windowGo D False)
  , ("M-k", windowGo U False)
  , ("M-h", windowGo L False)
  , ("M-l", windowGo R False)
  , ("M-S-j", windowSwap D False)
  , ("M-S-k", windowSwap U False)
  , ("M-S-h", windowSwap L False)
  , ("M-S-l", windowSwap R False)
  , ("M-m", windows W.focusMaster)
  , ("M-S-m", windows W.swapMaster)
  , ("M-n", windows W.focusDown)
  , ("M-S-n", windows W.focusUp)
  , ("M1-<Tab>", windows W.focusDown)
  , ("M1-S-<Tab>", windows W.focusUp)
  , ("M-<Backspace>", promote)

  -- Alternative windows navigation
  , ("M-<Down>", windowGo D False)
  , ("M-<Up>", windowGo U False)
  , ("M-<Left>", windowGo L False)
  , ("M-<Right>", windowGo R False)
  , ("M-S-<Down>", windowSwap D False)
  , ("M-S-<Up>", windowSwap U False)
  , ("M-S-<Left>", windowSwap L False)
  , ("M-S-<Right>", windowSwap R False)

    -- Urgent windows
  , ("M-u", focusUrgent)
  , ("M-S-u", nextMatch History (return True))

  -- Workspaces
  , ("M-<Tab>", nextWS)
  , ("M-S-<Tab>", prevWS)

    -- Layouts
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-C-h", sendMessage Shrink)
  , ("M-C-l", sendMessage Expand)
  , ("M-C-j", sendMessage MirrorShrink)
  , ("M-C-k", sendMessage MirrorExpand)
  , ("M-i", sendMessage (IncMasterN 1))
  , ("M-d", sendMessage (IncMasterN (-1)))
  , ("M-r", sendMessage (MT.Toggle REFLECTX))
  , ("M-b", sendMessage ToggleStruts)

  , ("M-a t", sendMessage $ JumpToLayout "tall")
  , ("M-a c", sendMessage $ JumpToLayout "columns")
  , ("M-a b", sendMessage $ JumpToLayout "bsp")
  , ("M-a m", sendMessage $ JumpToLayout "monocle")
  , ("M-f", sendMessage $ MT.Toggle NBFULL)

   -- SubLayouts
  , ("M-S-.", withFocused (sendMessage . mergeDir id))
  , ("M-S-,", withFocused (sendMessage . UnMerge))
  , ("M-.", onGroup W.focusUp')
  , ("M-,", onGroup W.focusDown')

    -- Scratchpads
  , ("M-s t", scratchTerm)
  , ("M-s v", scratchMixer)
  , ("M-s m", scratchMonitor)
  , ("M-s s", scratchPlayer)
  , ("M-s e", scratchNotes)

    -- Notifications
  , ("C-S-\\", spawn "dunstctl set-paused toggle")

    --- My Applications
  , ("M-M1-e", spawn myEditor)
  , ("M-M1-f", spawn myFileManager)
  , ("M-M1-b", spawn myBrowser)

    -- Multimedia Keys
  , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
  , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
  , ("<XF86AudioPlay>", spawn "playerctl --player=spotify,ncspot play-pause")
  , ("<XF86AudioNext>", spawn "playerctl --player=spotify,ncspot next")
  , ("<XF86AudioPrev>", spawn "playerctl --player=spotify,ncspot previous")
  , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
  , ("M-<Insert>", unGrab *> spawn "flameshot screen -p ~/Pictures/Screenshots")
  , ("M-S-<Insert>", unGrab *> spawn "flameshot gui")
  , ("M-<F3>", spawn "echo $(sxiv -t -o ~/Pictures/Wallpapers) > /home/alternateved/.cache/wall; xargs xwallpaper --stretch < ~/.cache/wall")

  ]
    -- Appending search engine prompts to keybindings list
  ++ [("M-<F2> " ++ k, S.promptSearch myXPConfig' f) | (k,f) <- searchList ]
  ++ [("M-S-<F2> " ++ k, S.selectSearch f) | (k,f) <- searchList ]
  ++ workspaceKeys
  ++ screenKeys
 where
  workspaceNumbers = [1 :: Int .. 9] <> [0]
  workspaceKeys =
    [ ("M-" <> m <> show k, withNthWorkspace f i)
    | (k, i) <- zip workspaceNumbers [0 ..]
    , (m, f) <- [("", W.greedyView), ("C-", W.view), ("S-", W.shift)]
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
  [ NS "terminal"   spawnTerm    findTerm    small
  , NS "volumectl"  spawnMixer   findMixer   small
  , NS "monitor"    spawnMonitor findMonitor medium
  , NS "player"     spawnPlayer  findPlayer  medium
  , NS "notes"      spawnNotes   findNotes   small
  ]
  where
    spawnTerm = myTerminal ++ " --title scratchpad"
    findTerm = title =? "scratchpad"

    spawnMixer = myTerminal ++ " --title pulsemixer -e pulsemixer"
    findMixer  = title =? "pulsemixer"

    spawnMonitor = myTerminal ++ " --title htop -e htop"
    findMonitor  = title =? "htop"

    spawnPlayer = myTerminal ++ " --title ncspot -e ncspot"
    findPlayer  = title =? "ncspot"

    spawnNotes = "emacsclient --eval '(open-scratch-frame)'"
    findNotes  = title =? "scratch"

    small = customFloating $ W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)
    medium = customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)
    large = customFloating $ W.RationalRect (1 / 10) (1 / 10) (4 / 5) (4 / 5)

scratchTerm, scratchMixer, scratchMonitor :: X ()
scratchTerm    = namedScratchpadAction myScratchPads "terminal"
scratchMixer   = namedScratchpadAction myScratchPads "volumectl"
scratchMonitor = namedScratchpadAction myScratchPads "monitor"
scratchPlayer  = namedScratchpadAction myScratchPads "player"
scratchNotes   = namedScratchpadAction myScratchPads "notes"

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
    , borderColor = colorFg
    , promptBorderWidth = 2
    , position = CenteredAt (2 / 4) (1 / 4)
    , height = 30
    , historySize = 100
    , historyFilter = id
    , defaultText = []
    , autoComplete = Just 100000
    , showCompletionOnTab = False
    , searchPredicate = fuzzyMatch
    , alwaysHighlight = True
    , maxComplRows = Just 5
    , promptKeymap = emacsLikeXPKeymap
    }

myXPConfig' :: XPConfig
myXPConfig' = myXPConfig
    { autoComplete = Nothing
    , historySize = 0
    }

------------------------------------------------------------------------
-- SEARCH ENGINES
------------------------------------------------------------------------
duckduckgo = S.searchEngine  "duckduckgo" "https://duckduckgo.com/?t=lm&q="
google     = S.searchEngine  "google"     "https://www.google.com/search?num=100&q="
hoogle     = S.searchEngine  "hoogle"     "https://www.haskell.org/hoogle/?hoogle="
reddit     = S.searchEngine  "reddit"     "https://www.reddit.com/search/?q="
url        = S.searchEngineF "url"        ("https://" <>)
wikipedia  = S.searchEngine  "wiki"       "https://en.wikipedia.org/wiki/Special:Search?go=Go&search="
youtube    = S.searchEngine  "youtube"    "https://www.youtube.com/results?search_type=search_videos&search_query="
nixpkgs    = S.searchEngine  "nixpkgs"    "https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query="
github     = S.searchEngine  "github"     "https://github.com/search?q="

searchList :: [(String, S.SearchEngine)]
searchList = [ ("d", duckduckgo)
             , ("o", google)
             , ("h", hoogle)
             , ("r", reddit)
             , ("u", url)
             , ("w", wikipedia)
             , ("y", youtube)
             , ("n", nixpkgs)
             , ("g", github)
             ]

-------------------------------------------------------------------------
-- 2D NAVIGATION
-------------------------------------------------------------------------
myNavigation2DConfig = def { defaultTiledNavigation = sideNavigation }

-------------------------------------------------------------------------
-- XMOBAR CONFIGURATION
-------------------------------------------------------------------------
mainXmobarPP :: ScreenId -> X PP
mainXmobarPP s = clickablePP . namedScratchpadFilterOutWorkspacePP $ def
      { ppCurrent = foreground . xmobarBorder "Bottom" colorFg 1
      , ppVisible = foreground
      , ppHidden = white
      , ppHiddenNoWindows = hiBlack
      , ppUrgent = red
      , ppTitle = foreground . shorten 60
      , ppSep = foreground " | "
      , ppExtras  = [ logLayoutOnScreen s
                    , shortenL 70 $ logTitleOnScreen s
                    ]
      , ppOrder = \(ws : _ : _ : extras) -> ws : extras
      }

auxXmobarPP :: ScreenId -> X PP
auxXmobarPP s = pure $ def
    { ppOrder  = \(_ : _ : _ : extras) -> extras
    , ppSep = foreground " | "
    , ppExtras = [ foregroundL $ logCurrentOnScreen s
                 , logLayoutOnScreen s
                 , shortenL 70 $ logTitleOnScreen s
                 ]
    }

-------------------------------------------------------------------------
-- XMOBAR INSTANCES
-------------------------------------------------------------------------
xmobar0, xmobar1 :: StatusBarConfig
xmobar0 = statusBarPropTo "xmobar0" "alternateved-xmobar"     (mainXmobarPP 0)
xmobar1 = statusBarPropTo "xmobar1" "alternateved-xmobar aux" (auxXmobarPP  1)

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner 0 = pure xmobar0
barSpawner 1 = pure xmobar1
barSpawner _ = mempty

-------------------------------------------------------------------------
-- HELPER FUNCTIONS
-------------------------------------------------------------------------
willFloat::Query Bool
willFloat = ask >>= \w -> liftX $ withDisplay $ \d -> do
  sh <- io $ getWMNormalHints d w
  let isFixedSize = isJust (sh_min_size sh) && sh_min_size sh == sh_max_size sh
  isTransient <- isJust <$> io (getTransientForHint d w)
  return (isFixedSize || isTransient)

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

basebg, basefg, base00, base07, base08, base01, base02, base04, base15 :: String
basebg = xprop "*.background"
basefg = xprop "*.foreground"
base00 = xprop "*.color0"
base07 = xprop "*.color7"
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
     . addRandrChangeHook (spawn "autorandr -c")
     . withNavigation2DConfig myNavigation2DConfig
     . withUrgencyHook NoUrgencyHook
     . dynamicSBs barSpawner
     $ myConfig
