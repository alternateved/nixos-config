-------------------------------------------------------------------------
-- IMPORTS
-------------------------------------------------------------------------

import System.Environment (getArgs)
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
    Monitors (Alsa, Battery, Cpu, Memory, Network, WeatherX),
    Runnable (..),
    StdinReader (UnsafeStdinReader),
    XMonadLog (UnsafeXPropertyLog),
    XPosition (OnScreen, TopW),
    defaultConfig,
    xmobar,
  )

-------------------------------------------------------------------------
-- MAIN
-------------------------------------------------------------------------
main :: IO ()
main = do
  xs <- getArgs
  case xs of
    ["aux"] -> xmobar auxConfig
    _ -> xmobar mainConfig

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
        lambdaIcon
          -- <> withPipe "%xmobar0% }{"
          <> withPipe "%UnsafeStdinReader% }{"
          <> "%EPLL% "
          <> withPipe "%notif%"
          <> withPipe "%wlp2s0%"
          <> withPipe (inIconFont "\xf2db" ++ " %cpu% ")
          <> withPipe (inIconFont "\xf538" ++ "%memory% ")
          <> withPipe (inIconFont "\xf028" ++ " %alsa:default:Master% ")
          <> withPipe "%battery% "
          <> withPipe "%date% "
    }

auxConfig :: Config
auxConfig =
  baseConfig
    { commands = auxCommands,
      position = OnScreen 1 (TopW L 100),
      template =
        lambdaIcon
          <> withPipe "%xmobar1% }"
          <> "{ %date% "
    }

-------------------------------------------------------------------------
-- COMMANDS
-------------------------------------------------------------------------
mainCommands :: [Runnable]
mainCommands =
  -- [ Run $ UnsafeXPropertyLog "xmobar0"
  [ Run UnsafeStdinReader,
    Run $
      WeatherX
        "EPLL"
        [ ("", inIconFont "\xf186"),
          ("clear", inIconFont "\xf185"),
          ("sunny", inIconFont "\xf185"),
          ("mostly clear", inIconFont "\xf185"),
          ("mostly sunny", inIconFont "\xf185"),
          ("partly sunny", inIconFont "\xf6c4"),
          ("fair", inIconFont "\xf186"),
          ("cloudy", inIconFont "\xf0c2"),
          ("overcast", inIconFont "\xf0c2"),
          ("partly cloudy", inIconFont "\xf6c4"),
          ("mostly cloudy", inIconFont "\xf0c2 "),
          ("considerable cloudiness", inIconFont "\xf740")
        ]
        [ "-t", "<fn=1><skyConditionS></fn> <tempC>°",
          "-L", "0",
          "-H", "25",
          "--low", colorBlue,
          "--normal", colorFg,
          "--high", colorRed
        ]
        36000,
    Run $ Com "bash" ["-c", "if [[ $(dunstctl is-paused) = false ]]; then echo '<fn=1>\xf0f3 </fn>'; else echo '<fn=1>\xf1f6 </fn>'; fi"] "notif" 1,
    Run $ Network "wlp2s0" ["-t", inIconFont "\xf063" ++ " <rx>kb " ++ inIconFont "\xf062" ++ " <tx>kb "] 20,
    Run $ Cpu [ "-L", "3", "-H", "50", "--high", colorRed, "-t", "<total>%"] 20,
    Run $ Memory ["-t", " <used>M (<usedratio>%)"] 20,
    Run $ Alsa "default" "Master"
        [ "--template",
          "<volumestatus>",
          "--suffix",
          "True",
          "--",
          "--on",
          "",
          "--off",
          "—"
        ],
    Run $ Battery
        [ "--template", "<acstatus>",
          "--Low", "20", -- units: %
          "--High", "95", -- units: %
          "--low", colorRed,
          "--high", colorFg,
          -- send message when low
          "-a", "dunstify -i 'dialog-warning' -u critical -t 3000 'Battery critically low'",
          "--", -- battery specific options
          -- discharging status
          "-o", inIconFont "\xf243" ++ " <left>% <timeleft>",
          -- AC "on" status
          "-O", inIconFont "\xf242" ++ " <left>%",
          -- charged status
          "-i", inIconFont "\xf240" ++ " 100%"
        ]
        150,
    Run $ Date "%H:%M" "date" 300
  ]

auxCommands :: [Runnable]
auxCommands =
  [ Run $ UnsafeXPropertyLog "xmobar1",
    Run $ Date "%H:%M" "date" 300
  ]

-------------------------------------------------------------------------
-- COLORS
-------------------------------------------------------------------------
colorBg, colorFg, colorRed, colorBlue, colorGreen :: String
colorBg = "#1d1f21"
colorFg = "#c4c8c5"
colorRed = "#cc6666"
colorBlue = "#80a1bd"
colorGreen = "#b5bd68"

-- OneDarker colors
-- colorBg, colorFg, colorRed, colorBlue, colorGreen :: String
-- colorBg     = "#1E222A"
-- colorFg     = "#C8CCD4"
-- colorRed    = "#D47D85"
-- colorBlue   = "#61afef"
-- colorGreen  = "#7eca9c"

red, blue :: String -> String
red = xmobarColor colorRed ""
blue = xmobarColor colorBlue ""

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

-------------------------------------------------------------------------
-- FONTS AND ICONS
-------------------------------------------------------------------------
mainFont :: String
mainFont = "xft:JetBrainsMono Nerd Font:weight=regular:pixelsize=14:antialias=true:hinting=true"

iconFont :: String
iconFont = "xft:Font Awesome 5 Free Solid:pixelsize=14"

-- Wrap stuff so it uses the icon font.
inIconFont :: String -> String
inIconFont = wrap "<fn=1>" "</fn>"

lambdaIcon :: String
lambdaIcon = " λ "

withPipe :: String -> String
withPipe = (++) "| "
