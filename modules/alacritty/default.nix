let colors = import ../theme/colors.nix;
in {
  home-manager.users.alternateved.programs.alacritty = {
    enable = true;
    settings = {
      background_opacity = 1.0;
      window = {
        padding = {
          x = 10;
          y = 10;
        };
        decorations = "none";
      };
      font = {
        # size = 9.0;
        size = 13.0;
        normal.family = "Iosevka Nerd Font";
        bold.family = "Iosevka Nerd Font";
        italic.family = "Iosevka Nerd Font";
      };
      cursor.style = {
        shape = "Underline";
        blinking = "On";
      };
      draw_bold_text_with_bright_colors = true;
      colors = {
        primary = {
          background = colors.background;
          foreground = colors.foreground;
        };
        cursor = {
          text = colors.black;
          cursor = colors.white;
        };
        normal = {
          black = colors.black;
          red = colors.red;
          green = colors.green;
          yellow = colors.yellow;
          blue = colors.blue;
          magenta = colors.magenta;
          cyan = colors.cyan;
          white = colors.white;
        };
        bright = {
          black = colors.bright-black;
          red = colors.bright-red;
          green = colors.bright-green;
          yellow = colors.bright-yellow;
          blue = colors.bright-blue;
          magenta = colors.bright-magenta;
          cyan = colors.bright-cyan;
          white = colors.bright-white;
        };
      };
      mouse.hide_when_typing = true;
      key_bindings = [
        {
          key = "V";
          mods = "Control|Shift";
          action = "Paste";
        }
        {
          key = "C";
          mods = "Control|Shift";
          action = "Copy";
        }
        {
          key = "Insert";
          mods = "Shift";
          action = "PasteSelection";
        }
        {
          key = "Key0";
          mods = "Control";
          action = "ResetFontSize";
        }
        {
          key = "Equals";
          mods = "Control";
          action = "IncreaseFontSize";
        }
        {
          key = "Plus";
          mods = "Control";
          action = "IncreaseFontSize";
        }
        {
          key = "NumpadAdd";
          mods = "Control";
          action = "IncreaseFontSize";
        }
        {
          key = "Minus";
          mods = "Control";
          action = "DecreaseFontSize";
        }
        {
          key = "NumpadSubtract";
          mods = "Control";
          action = "DecreaseFontSize";
        }
        {
          key = "F11";
          mods = "None";
          action = "ToggleFullscreen";
        }
        {
          key = "Paste";
          mods = "None";
          action = "Paste";
        }
        {
          key = "Copy";
          mods = "None";
          action = "Copy";
        }
        {
          key = "L";
          mods = "Control";
          action = "ClearLogNotice";
        }
        {
          key = "L";
          mods = "Control";
          chars = "x0c";
        }
        {
          key = "PageUp";
          mods = "None";
          action = "ScrollPageUp";
          mode = "~Alt";
        }
        {
          key = "K";
          mods = "Control|Shift";
          action = "ScrollPageUp";
          mode = "~Alt";
        }
        {
          key = "J";
          mods = "Control|Shift";
          action = "ScrollPageDown";
          mode = "~Alt";
        }
        {
          key = "PageDown";
          mods = "None";
          action = "ScrollPageDown";
          mode = "~Alt";
        }
        {
          key = "Home";
          mods = "Shift";
          action = "ScrollToTop";
          mode = "~Alt";
        }
        {
          key = "End";
          mods = "Shift";
          action = "ScrollToBottom";
          mode = "~Alt";
        }
      ];
    };
  };
}
