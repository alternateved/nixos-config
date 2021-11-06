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
        # size = 12.0;
        size = 8.5;
        normal.family = "Iosevka Nerd Font";
        bold.family = "Iosevka Nerd Font";
        italic.family = "Iosevka Nerd Font";
      };
      cursor.style = {
        shape = "Block";
        blinking = "On";
      };
      draw_bold_text_with_bright_colors = true;
      colors = {
        primary = {
          background = colors.background;
          foreground = colors.foreground;
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
    };
  };
}
