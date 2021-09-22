-------------------------------------------------------------------------
-- IMPORTS
-------------------------------------------------------------------------
module Main (main) where

-- Data
import Data.Bifunctor (bimap)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, elemIndex, find)
import Data.Maybe (catMaybes, fromMaybe)
-- System
import System.Environment (getArgs)
import System.IO.Unsafe (unsafeDupablePerformIO)
-- Base
import Xmobar
import XMonad.Util.Run (runProcessWithInput)

-------------------------------------------------------------------------
-- MAIN
-------------------------------------------------------------------------
main :: IO ()
main = do
    xmobar mainConfig

-------------------------------------------------------------------------
-- CONFIG
-------------------------------------------------------------------------
baseConfig :: Config
baseConfig =
  defaultConfig
    { font = mainFont,
      bgColor = colorBg,
      fgColor = colorFg ,
      lowerOnStart = True,
      hideOnStart = False,
      allDesktops = True,
      persistent = False,
      sepChar = "%",
      alignSep = "}{"
    }

mainConfig :: Config
mainConfig =
  baseConfig
    { commands = mainCommands,
      position = OnScreen 0 (TopW L 100),
      template =
        " %UnsafeXMonadLog% }{"
          <> "%notif%"
          <> withPipe "%EPLL%"
          <> withPipe "%cpu%"
          <> withPipe "%memory%"
          <> withPipe "%default:Master%"
          <> withPipe "%battery%"
          <> withPipe "%date%"
          <> withPipe "%time% "
    }

-------------------------------------------------------------------------
-- COMMANDS
-------------------------------------------------------------------------
mainCommands :: [Runnable]
mainCommands =
  [ Run $ UnsafeXMonadLog,
    Run $ Com "bash" ["-c", "if [[ $(dunstctl is-paused) = false ]]; then echo '\xf0f3 '; else echo '\xf1f6 '; fi"] "notif" 20,
    Run $ Weather "EPLL"
        [ "--template", "<weather> <tempC>°C",
          "-L", "0",
          "-H", "25",
          "--low"   , colorBlue,
          "--normal", colorFg,
          "--high"  , colorRed
        ] 36000,
    Run $ Cpu [ "-L", "3", "-H", "50", "--high", colorRed, "-t", "\xf2db  <total>%"] 20,
    Run $ Memory ["-t", "\xf85a  <used>M (<usedratio>%)"] 20,
    Run $ Volume "default" "Master"
        [ "--template", "<volumestatus>",
          "--suffix"  , "True",  -- Show "%" at the end of the <volume> string.
          "--",                  -- Volume specific options.
          "--on"     , "",
          "--off"    , "\xfc5d  OFF",
          "--lowv"   , "20",        -- Low  threshold for strings (in %).
          "--highv"  , "60",        -- High threshold for strings (in %).
          "--lows"   , "\xfa7e  ",   -- Low
          "--mediums", "\xfa7f  ",   -- Medium
          "--highs"  , "\xfa7d  ",   -- High
          "--onc"    , colorFg,     -- On  color.
          "--offc"   , colorRed     -- Off color.
        ] 10,
    Run $ Battery
        [ "--template", "<acstatus>",
          "--Low", "20", -- units: %
          "--High", "95", -- units: %
          "--low", colorRed,
          "--high", colorFg,
          -- send message when low
          "--", -- battery specific options
          -- discharging status
          "-o", "\xf243   <left>% <timeleft>",
          -- AC "on" status
          "-O", "\xf242   <left>%",
          -- charged status
          "-i", "\xf240   100%",
          "-a", "dunstify -u critical 'Battery' 'Battery running out!'"
        ] 150,
    Run $ Date "%A, %b %_d" "date" 500,
    Run $ Date "%H:%M" "time" 300
  ]

-------------------------------------------------------------------------
-- COLORS
-------------------------------------------------------------------------
colorBg, colorFg, colorHiWhite, colorLoGray, colorHiGray, colorRed, colorBlue, colorGreen :: String
colorBg       = basebg
colorFg       = basefg
colorHiWhite  = base15
colorLoGray   = base00
colorHiGray   = base08
colorRed      = base01
colorBlue     = base04
colorGreen    = base02

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
mainFont = "xft:Iosevka Nerd Font:style=medium:pixelsize=14:antialias=true:hinting=true"

withPipe :: String -> String
withPipe = (++) " | "
