-------------------------------------------------------------------------
-- IMPORTS
-------------------------------------------------------------------------

import System.Environment (getArgs)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Data.Bifunctor (bimap)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, elemIndex, find)
import Data.Maybe (catMaybes, fromMaybe)
import Xmobar
  ( Align (L),
    Command (Com),
    Config
      ( additionalFonts,
        alignSep,
        allDesktops,
        bgColor,
        commands,
        fgColor,
        font,
        hideOnStart,
        lowerOnStart,
        persistent,
        position,
        sepChar,
        template
      ),
    Date (Date),
    Monitors (Alsa, Battery, Cpu, Memory, Network, Weather),
    Runnable (..),
    StdinReader (UnsafeStdinReader),
    XMonadLog (UnsafeXPropertyLog),
    XPosition (OnScreen, TopW),
    defaultConfig,
    xmobar,
  )
import XMonad.Util.Run (runProcessWithInput)

-------------------------------------------------------------------------
-- MAIN
-------------------------------------------------------------------------
main :: IO ()
main = do
  xs <- getArgs
  case xs of
    ["aux"] -> xmobar auxConfig
    _       -> xmobar mainConfig

-------------------------------------------------------------------------
-- CONFIG
-------------------------------------------------------------------------
baseConfig :: Config
baseConfig =
  defaultConfig
    { font = mainFont,
      additionalFonts = [iconFont],
      bgColor = colorBg,
      fgColor = colorFg,
      lowerOnStart = True,
      hideOnStart = False,
      allDesktops = True,
      persistent = True,
      sepChar = "%",
      alignSep = "}{"
    }

mainConfig :: Config
mainConfig =
  baseConfig
    { commands = mainCommands,
      position = OnScreen 0 (TopW L 100),
      template =
        " %xmobar0% }{"
          <> "%notif% "
          <> withPipe "%EPLL% "
          <> withPipe (inIconFont "\xf2db" ++ " %cpu% ")
          <> withPipe (inIconFont "\xf538" ++ "%memory% ")
          <> withPipe "%battery% "
          <> withPipe "%date% "
          <> withPipe "%time% "
    }

auxConfig :: Config
auxConfig =
  baseConfig
    { commands = auxCommands,
      position = OnScreen 1 (TopW L 100),
      template =
        "%xmobar1% }"
          <> "{ %date% "
          <> withPipe "%time% "
    }

-------------------------------------------------------------------------
-- COMMANDS
-------------------------------------------------------------------------
mainCommands :: [Runnable]
mainCommands =
  [ Run $ UnsafeXPropertyLog "xmobar0",
    Run $ Com "bash" ["-c", "if [[ $(dunstctl is-paused) = false ]]; then echo '<fn=1>\xf0f3</fn>'; else echo '<fn=1>\xf1f6</fn>'; fi"] "notif" 1,
    Run $ Weather "EPLL"
        [ "--template", "<weather> <tempC>°C",
          "-L", "0",
          "-H", "25",
          "--low"   , colorBlue,
          "--normal", colorFg,
          "--high"  , colorRed
        ] 36000,
    Run $ Cpu [ "-L", "3", "-H", "50", "--high", colorRed, "-t", "<total>%"] 20,
    Run $ Memory ["-t", " <used>M (<usedratio>%)"] 20,
    Run $ Battery
        [ "--template", "<acstatus>",
          "--Low", "20", -- units: %
          "--High", "95", -- units: %
          "--low", colorRed,
          "--high", colorFg,
          -- send message when low
          "--", -- battery specific options
          -- discharging status
          "-o", inIconFont "\xf243" ++ " <left>% <timeleft>",
          -- AC "on" status
          "-O", inIconFont "\xf242" ++ " <left>%",
          -- charged status
          "-i", inIconFont "\xf240" ++ " 100%"
        ] 150,
    Run $ Date "%A, %b %_d" "date" 500,
    Run $ Date "%H:%M" "time" 300
  ]

auxCommands :: [Runnable]
auxCommands =
  [ Run $ UnsafeXPropertyLog "xmobar1",
    Run $ Date "%A, %b %_d" "date" 500,
    Run $ Date "%H:%M" "time" 300
  ]

-------------------------------------------------------------------------
-- COLORS
-------------------------------------------------------------------------
colorBg, colorFg, colorHiWhite, colorLoGrey, colorHiGrey, colorRed, colorBlue, colorGreen :: String
colorBg       = basebg
colorFg       = basefg
colorHiWhite  = base15
colorLoGrey   = base00
colorHiGrey   = base08
colorRed      = base01
colorBlue     = base04
colorGreen    = base02

red, blue :: String -> String
red   = xmobarColor colorRed ""
blue  = xmobarColor colorBlue ""

-------------------------------------------------------------------------
-- HELPER FUNCTIONS
-------------------------------------------------------------------------
-- Use xmobar escape codes to output a string with given foreground and background colors.
-- Source: https://hackage.haskell.org/package/xmonad-contrib-0.15/docs/src/XMonad.Hooks.DynamicLog.html#xmobarColor
xmobarColor ::
  -- | foreground color: a color name, or #rrggbb format
  String ->
  -- | background color
  String ->
  -- | output string
  String ->
  String
xmobarColor fg bg = wrap open "</fc>"
  where
    open :: String
    open = concat ["<fc=", fg, if null bg then "" else "," <> bg, ">"]

-- Wrap a string in delimiters, unless it is empty.
-- Source: https://hackage.haskell.org/package/xmonad-contrib-0.15/docs/src/XMonad.Hooks.DynamicLog.html#wrap
wrap ::
  -- | left delimiter
  String ->
  -- | right delimiter
  String ->
  -- | output string
  String ->
  String
wrap _ _ "" = ""
wrap l r m = l <> m <> r

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
-- FONTS AND ICONS
-------------------------------------------------------------------------
mainFont :: String
mainFont = "xft:Iosevka Nerd Font Mono:style=medium:pixelsize=15:antialias=true:hinting=true"

iconFont :: String
iconFont = "xft:Font Awesome 5 Free Solid:pixelsize=14"

-- Wrap stuff so it uses the icon font.
inIconFont :: String -> String
inIconFont = wrap "<fn=1>" "</fn>"

withPipe :: String -> String
withPipe = (++) "| "
