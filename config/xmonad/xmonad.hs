------------------------------------------------------------------------
-- IMPORTS
-------------------------------------------------------------------------
module Main (main) where

-- Data
import Data.Bifunctor (bimap)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, elemIndex, find)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.Monoid (All (..))
-- System
import System.IO.Unsafe (unsafeDupablePerformIO)
-- Base
import XMonad
-- Actions
import XMonad.Actions.CopyWindow (copy, kill1)
import XMonad.Actions.CycleRecentWS (toggleRecentWS)
import XMonad.Actions.CycleWS (Direction1D (..), WSType (..), emptyWS, ignoringWSs, moveTo)
import XMonad.Actions.DynamicWorkspaceOrder (getSortByOrder)
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeWorkspace, renameWorkspace, selectWorkspace, withNthWorkspace)
import XMonad.Actions.EasyMotion (EasyMotionConfig (..), selectWindow, textSize)
import XMonad.Actions.GroupNavigation (Direction (History), historyHook, nextMatch)
import XMonad.Actions.KeyRemap (KeymapTable (..), buildKeyRemapBindings, emptyKeyRemap, setDefaultKeyRemap)
import XMonad.Actions.Promote (promote)
import XMonad.Actions.RotSlaves (rotSlavesDown, rotSlavesUp)
import qualified XMonad.Actions.Search as S (SearchEngine (..), duckduckgo, github, google, hoogle, promptSearch, searchEngine, searchEngineF, selectSearch, wikipedia, youtube)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.WithAll (sinkAll)
-- Hooks
import XMonad.Hooks.DynamicProperty (dynamicPropertyChange)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen, setEwmhActivateHook)
import XMonad.Hooks.InsertPosition (Focus (Newer), Position (Below), insertPosition)
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docks)
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, isDialog)
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.StatusBar (StatusBarConfig, statusBarProp, withSB)
import XMonad.Hooks.StatusBar.PP hiding (trim)
import XMonad.Hooks.UrgencyHook (NoUrgencyHook (NoUrgencyHook), doAskUrgent, focusUrgent, withUrgencyHook)
-- Layouts
import XMonad.Layout.Grid (Grid (..))
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Magnifier (MagnifyThis (NoMaster), magnify)
import XMonad.Layout.MultiToggle (EOT (EOT), mkToggle, single, (??))
import qualified XMonad.Layout.MultiToggle as MT (Toggle (..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Reflect (REFLECTX (..))
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.ResizableTile (MirrorResize (..), ResizableTall (ResizableTall))
import XMonad.Layout.Spacing (Border (Border), Spacing, spacingRaw)
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Layout.TwoPane (TwoPane (..))
import XMonad.Layout.WorkspaceDir (changeDir, workspaceDir)
import XMonad.Operations
-- Prompt
import XMonad.Prompt (XPConfig (..), XPPosition (CenteredAt), deleteAllDuplicates, emacsLikeXPKeymap)
import XMonad.Prompt.DirExec (dirExecPromptNamed)
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)
import XMonad.Prompt.Man (manPrompt)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Window (WindowPrompt (Bring, Goto), allWindows, windowPrompt, wsWindows)
import qualified XMonad.StackSet as W
-- Utilities
import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP, removeKeysP)
import XMonad.Util.Loggers (logTitles)
import XMonad.Util.NamedScratchpad (NamedScratchpad (NS), customFloating, namedScratchpadAction, namedScratchpadFilterOutWorkspacePP, namedScratchpadManageHook, scratchpadWorkspaceTag)
import XMonad.Util.Run (runProcessWithInput)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Ungrab (unGrab)
import XMonad.Util.WorkspaceCompare (filterOutWs)

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
myFont = "xft:JetBrainsMono Nerd Font Mono:style=regular:size=11:antialias=true:hinting=true"

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

macMap :: KeymapTable
macMap =
  KeymapTable
    [ ((myModMask, xK_x), (controlMask, xK_x)),
      ((myModMask, xK_c), (controlMask, xK_c)),
      ((myModMask, xK_v), (controlMask, xK_v)),
      ((myModMask, xK_f), (controlMask, xK_f))
    ]

-------------------------------------------------------------------------
-- COLORS
-------------------------------------------------------------------------
-- Window colors
myNormColor :: String
myNormColor = colorBg

myFocusColor :: String
myFocusColor = colorFg

-- Base colors
colorBg, colorFg, colorWhite, colorHiWhite, colorBlack, colorHiBlack, colorRed, colorBlue, colorGreen :: String
colorBg = basebg
colorFg = basefg
colorWhite = base07
colorHiWhite = base15
colorBlack = base00
colorHiBlack = base08
colorRed = base01
colorBlue = base04
colorGreen = base02

white, hiWhite, foreground, black, hiBlack, red :: String -> String
foreground = xmobarColor colorFg ""
white = xmobarColor colorWhite ""
hiWhite = xmobarColor colorHiWhite ""
black = xmobarColor colorBlack ""
hiBlack = xmobarColor colorHiBlack ""
red = xmobarColor colorRed ""

-------------------------------------------------------------------------
-- STARTUPHOOK
-------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawnOnce "xrdb -merge ~/.Xresources"
  spawnOnce "xargs xwallpaper --stretch < ~/.cache/wall"
  setDefaultKeyRemap macMap [macMap, emptyKeyRemap]
  setWMName "LG3D"

-------------------------------------------------------------------------
-- MANAGEHOOK
-------------------------------------------------------------------------
myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Firefox" --> doShift (myWorkspaces !! 0),
      className =? "Thunderbird" --> doShift (myWorkspaces !! 2),
      className =? "Signal" --> doShift (myWorkspaces !! 1),
      className =? "discord" --> doShift (myWorkspaces !! 1),
      className =? "mpv" --> doShift (myWorkspaces !! 4),
      className =? "Peek" --> doCenterFloat,
      className =? "Sxiv" --> doCenterFloat,
      isDialog --> doCenterFloat,
      fmap not willFloat --> insertPosition Below Newer
    ]
    <+> namedScratchpadManageHook myScratchPads

-------------------------------------------------------------------------
-- EVENTHOOK
-------------------------------------------------------------------------
myHandleEventHook :: Event -> X All
myHandleEventHook = dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> doShift (myWorkspaces !! 4)) <+> restartEventHook

-------------------------------------------------------------------------
-- NIXOS RESTARTHOOK
-------------------------------------------------------------------------
restartEventHook e@ClientMessageEvent {ev_message_type = mt} = do
  a <- getAtom "XMONAD_RESTART"
  if mt == a
    then XMonad.Operations.restart "alternateved-xmonad" True >> return (All True)
    else return $ All True
restartEventHook _ = return $ All True

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
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1", "2", "3", "4", "5"]

-------------------------------------------------------------------------
-- LAYOUTS
-------------------------------------------------------------------------
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw True (Border 0 i 0 i) True (Border i 0 i 0) True

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
          ResizableTall 1 (3 / 100) (1 / 2) []

panes =
  renamed [Replace "panes"] $
    avoidStruts $
      mySpacing 5 $
        TwoPane (3 / 100) (1 / 2)

englare =
  renamed [Replace "englare"] $
    avoidStruts $
      mySpacing 5 $
        magnify 1.3 (NoMaster 3) True $
          ResizableTall 1 (3 / 100) (1 / 2) []

columns =
  renamed [Replace "columns"] $
    avoidStruts $
      mySpacing 5 $
        ThreeColMid 1 (3 / 100) (12 / 30)

grid =
  renamed [Replace "grid"] $
    avoidStruts $
      mySpacing 5 $
        GridRatio (4 / 3)

monocle =
  renamed [Replace "monocle"] $
    avoidStruts $
      Full

myLayoutHook =
  workspaceDir myHome $
    smartBorders $
      mkToggle (single REFLECTX) $
        mkToggle (NBFULL ?? NOBORDERS ?? EOT) $
          myDefaultLayout
  where
    myDefaultLayout =
      tall
        ||| wide
        ||| panes
        ||| englare
        ||| columns
        ||| grid
        ||| monocle

-------------------------------------------------------------------------
-- KEYBINDINGS
-------------------------------------------------------------------------
myKeys :: [(String, X ())]
myKeys =
  -- Xmonad
  [ ("M-S-r", spawn "xmonad --restart"),
    ("M-C-r", spawn $ myEditor ++ xmonadConfig),
    ("M-C-b", spawn $ myEditor ++ xmobarConfig),
    -- Open preferred terminal
    ("M-S-<Return>", spawn myTerminal),
    -- Prompts
    ("M-p", shellPrompt myXPConfig),
    ("M-S-q", dirExecPromptNamed myXPConfig' spawn (myDots ++ "/xmonad/scripts") "Session: "),
    ("M-<F1>", manPrompt myXPConfig),
    ("M-'", windowPrompt myXPConfig' Goto wsWindows),
    ("M-C-'", windowPrompt myXPConfig' Goto allWindows),
    ("M-S-'", windowPrompt myXPConfig' Bring allWindows),
    ("M-o", selectWindow emConfig >>= (`whenJust` windows . W.focusWindow)),
    -- Workspace management
    ("M-y a", addWorkspacePrompt myXPConfig'),
    ("M-y s", selectWorkspace myXPConfig),
    ("M-y r", renameWorkspace myXPConfig'),
    ("M-y c", changeDir myXPConfig'),
    ("M-y d", removeWorkspace),
    -- Windows
    ("M-S-c", kill1),
    -- Floating windows
    ("M-t", withFocused toggleFloat),
    ("M-S-t", sinkAll),
    -- Windows navigation
    ("M-j", windows W.focusDown),
    ("M-k", windows W.focusUp),
    ("M-m", windows W.focusMaster),
    ("M-<Return>", promote),
    -- Urgent windows
    ("M-u", focusUrgent),
    ("M-S-u", nextMatch History (return True)),
    -- Workspace/window/screen focus changes
    ("M-<Tab>", moveTo Next $ Not emptyWS :&: ignoringWSs [scratchpadWorkspaceTag]),
    ("M-S-<Tab>", moveTo Prev $ Not emptyWS :&: ignoringWSs [scratchpadWorkspaceTag]),
    ("M-`", toggleRecentWS),
    ("M1-`", nextMatch History (return True)),
    ("M1-<Tab>", windows W.focusDown),
    ("M1-S-<Tab>", windows W.focusUp),
    ("M-<Backspace>", rotSlavesUp),
    ("M-S-<Backspace>", rotSlavesDown),
    -- Layouts
    ("M-<Space>", sendMessage NextLayout),
    ("M-C-h", sendMessage Shrink),
    ("M-C-l", sendMessage Expand),
    ("M-C-j", sendMessage MirrorShrink),
    ("M-C-k", sendMessage MirrorExpand),
    ("M-i", sendMessage (IncMasterN 1)),
    ("M-d", sendMessage (IncMasterN (-1))),
    ("M-r", sendMessage (MT.Toggle REFLECTX)),
    ("M-b", sendMessage ToggleStruts),
    ("M-a t", sendMessage $ JumpToLayout "tall"),
    ("M-a w", sendMessage $ JumpToLayout "wide"),
    ("M-a p", sendMessage $ JumpToLayout "panes"),
    ("M-a e", sendMessage $ JumpToLayout "englare"),
    ("M-a c", sendMessage $ JumpToLayout "columns"),
    ("M-a g", sendMessage $ JumpToLayout "grid"),
    ("M-a m", sendMessage $ JumpToLayout "monocle"),
    ("M-S-f", sendMessage $ MT.Toggle NBFULL),
    -- Scratchpads
    ("M-s t", scratchTerm),
    ("M-s v", scratchMixer),
    ("M-s m", scratchMonitor),
    ("M-s s", scratchPlayer),
    -- Notifications
    ("C-S-\\", spawn "dunstctl set-paused toggle"),
    --- My Applications
    ("M-C-e", spawn myEditor),
    ("M-M1-e", spawn "emacs"),
    ("M-M1-f", spawn myFileManager),
    ("M-M1-b", spawn myBrowser),
    -- Multimedia Keys
    ("<XF86AudioMute>", spawn "pamixer -t"),
    ("<XF86AudioLowerVolume>", spawn "pamixer -d 10"),
    ("<XF86AudioRaiseVolume>", spawn "pamixer -i 10"),
    ("S-<XF86AudioRaiseVolume>", spawn "pamixer -i 10 --allow-boost"),
    ("<XF86AudioPlay>", spawn "playerctl play-pause"),
    ("<XF86AudioNext>", spawn "playerctl next"),
    ("<XF86AudioPrev>", spawn "playerctl previous"),
    ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10"),
    ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10"),
    ("M-<Insert>", unGrab *> spawn "flameshot screen -p ~/Pictures/Screenshots"),
    ("M-S-<Insert>", unGrab *> spawn "flameshot gui"),
    ("M-<F3>", spawn "echo $(sxiv -t -o ~/Pictures/Wallpapers) > /home/alternateved/.cache/wall; xargs xwallpaper --stretch < ~/.cache/wall")
  ]
    -- Appending search engine prompts to keybindings list
    ++ [("M-<F2> " ++ k, S.promptSearch myXPConfig' f) | (k, f) <- searchList]
    ++ [("M-S-<F2> " ++ k, S.selectSearch f) | (k, f) <- searchList]
    ++ workspaceKeys
  where
    workspaceNumbers = [1 :: Int .. 9] <> [0]
    workspaceKeys =
      [ ("M-" <> m <> show k, withNthWorkspace' f i)
        | (k, i) <- zip workspaceNumbers [0 ..],
          (m, f) <- [("", W.view), ("S-", W.shift), ("C-", copy)]
      ]
    -- Re-implementation of withNthworkspace with "skipTags" added to filter out NSP
    withNthWorkspace' :: (String -> WindowSet -> WindowSet) -> Int -> X ()
    withNthWorkspace' job wnum = do
      sort <- getSortByOrder
      ws <- gets (map W.tag . sort . filterOutWs [scratchpadWorkspaceTag] . W.workspaces . windowset)
      case drop wnum ws of
        (w : _) -> windows $ job w
        [] -> return ()

-------------------------------------------------------------------------
-- SCRATCHPADS
-------------------------------------------------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "terminal" spawnTerm findTerm small,
    NS "volumectl" spawnMixer findMixer wide,
    NS "monitor" spawnMonitor findMonitor large,
    NS "player" spawnPlayer findPlayer wider
  ]
  where
    spawnTerm = myTerminal ++ " --title scratchpad"
    findTerm = title =? "scratchpad"

    spawnMixer = myTerminal ++ " --title pulsemixer -e pulsemixer"
    findMixer = title =? "pulsemixer"

    spawnMonitor = myTerminal ++ " --title htop -e htop"
    findMonitor = title =? "htop"

    spawnPlayer = myTerminal ++ " --title ncspot -e ncspot"
    findPlayer = title =? "ncspot"

    small = customFloating $ W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)
    medium = customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)
    large = customFloating $ W.RationalRect (1 / 10) (1 / 10) (4 / 5) (4 / 5)
    wide = customFloating $ W.RationalRect (1 / 6) (2 / 6) (2 / 3) (1 / 3)
    wider = customFloating $ W.RationalRect (1 / 6) (2 / 6) (2 / 3) (2 / 5)

scratchTerm, scratchMixer, scratchMonitor, scratchPlayer :: X ()
scratchTerm = namedScratchpadAction myScratchPads "terminal"
scratchMixer = namedScratchpadAction myScratchPads "volumectl"
scratchMonitor = namedScratchpadAction myScratchPads "monitor"
scratchPlayer = namedScratchpadAction myScratchPads "player"

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
      borderColor = colorFg,
      promptBorderWidth = 1,
      -- position = CenteredAt (2 / 4) (2 / 6),
      position = CenteredAt (1 / 4) (3 / 7),
      height = 30,
      historySize = 50,
      historyFilter = deleteAllDuplicates,
      defaultText = [],
      autoComplete = Nothing,
      showCompletionOnTab = False,
      searchPredicate = fuzzyMatch,
      sorter = fuzzySort,
      alwaysHighlight = True,
      maxComplRows = Just 5,
      promptKeymap = emacsLikeXPKeymap
    }

myXPConfig' :: XPConfig
myXPConfig' =
  myXPConfig
    { historySize = 0
    }

-------------------------------------------------------------------------
-- EASYMOTION CONFIGURATION
-------------------------------------------------------------------------
emConfig :: EasyMotionConfig
emConfig =
  def
    { txtCol = colorFg,
      bgCol = colorBg,
      borderCol = colorFg,
      overlayF = textSize,
      cancelKey = xK_Escape,
      emFont = myFont,
      borderPx = 1
    }

------------------------------------------------------------------------
-- SEARCH ENGINES
------------------------------------------------------------------------
nixpkgs, reddit, url :: S.SearchEngine
nixpkgs = S.searchEngine "nixpkgs" "https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query="
reddit = S.searchEngine "reddit" "https://www.reddit.com/search/?q="
url = S.searchEngineF "url" ("https://" <>)

searchList :: [(String, S.SearchEngine)]
searchList =
  [ ("d", S.duckduckgo),
    ("o", S.google),
    ("h", S.hoogle),
    ("w", S.wikipedia),
    ("y", S.youtube),
    ("g", S.github),
    ("n", nixpkgs),
    ("r", reddit),
    ("u", url)
  ]

-------------------------------------------------------------------------
-- XMOBAR CONFIGURATION
-------------------------------------------------------------------------
mainXmobarPP :: X PP
mainXmobarPP =
  clickablePP . filterOutWsPP [scratchpadWorkspaceTag] $
    def
      { ppCurrent = foreground . xmobarBorder "Bottom" colorFg 2 . wrap " " " ",
        ppVisible = foreground . wrap " " " ",
        ppHidden = white . wrap " " " ",
        ppHiddenNoWindows = hiBlack . wrap " " " ",
        ppUrgent = red . wrap " " " ",
        ppTitle = foreground . shorten 70,
        ppSep = foreground " | ",
        ppOrder = \(ws : l : t : extras) -> ws : l : t : extras,
        ppExtras = []
      }

-------------------------------------------------------------------------
-- XMOBAR INSTANCES
-------------------------------------------------------------------------
xmobar0 :: StatusBarConfig
xmobar0 = statusBarProp "alternateved-xmobar" mainXmobarPP

-------------------------------------------------------------------------
-- HELPER FUNCTIONS
-------------------------------------------------------------------------
willFloat :: Query Bool
willFloat =
  ask >>= \w -> liftX $
    withDisplay $ \d -> do
      sh <- io $ getWMNormalHints d w
      let isFixedSize = isJust (sh_min_size sh) && sh_min_size sh == sh_max_size sh
      isTransient <- isJust <$> io (getTransientForHint d w)
      return (isFixedSize || isTransient)

toggleFloat :: Window -> X ()
toggleFloat w =
  windows
    ( \s ->
        if M.member w (W.floating s)
          then W.sink w s
          else (W.float w (W.RationalRect (1 / 3) (1 / 4) (1 / 2) (1 / 2)) s)
    )

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
      handleEventHook = myHandleEventHook,
      workspaces = myWorkspaces,
      borderWidth = myBorderWidth,
      normalBorderColor = myNormColor,
      focusedBorderColor = myFocusColor
    }
    `additionalKeys` buildKeyRemapBindings [macMap, emptyKeyRemap]
    `additionalKeysP` myKeys
    `removeKeysP` ["M-h", "M-l"]

-------------------------------------------------------------------------
-- MAIN
-------------------------------------------------------------------------
main :: IO ()
main =
  xmonad
    . withSB xmobar0
    . docks
    . setEwmhActivateHook doAskUrgent
    . ewmh
    . ewmhFullscreen
    . withUrgencyHook NoUrgencyHook
    $ myConfig
