{ pkgs, ... }:
let colors = import ../theme/colors.nix;
in {
  programs.rofi = {
    enable = true;
    font = "Iosevka Nerd Font 12";
    terminal = "${pkgs.alacritty}/bin/alacritty";
    borderWidth = 3;
    lines = 10;
    separator = "solid";
    scrollbar = false;
    colors = {
      window = {
        background = colors.background;
        border = colors.bright-black;
        separator = colors.black;
      };

      rows = {
        normal = {
          background = colors.background;
          backgroundAlt = colors.background;
          foreground = colors.foreground;
          highlight = {
            background = colors.foreground;
            foreground = colors.background;
          };
        };
        active = {
          background = colors.background;
          backgroundAlt = colors.background;
          foreground = colors.green;
          highlight = {
            background = colors.green;
            foreground = colors.background;
          };
        };
        urgent = {
          background = colors.background;
          backgroundAlt = colors.background;
          foreground = colors.red;
          highlight = {
            background = colors.red;
            foreground = colors.background;
          };
        };
      };
    };
    extraConfig = {
      modi = "drun,run,window,windowcd";
      icon-theme = "Tela black dark";
      cycle = true;
      disable-history = false;
      kb-accept-entry = "Return,Control+m,KP_Enter";
      kb-row-down = "Down,Control+n,Control+j";
      kb-remove-to-eol = "";
      kb-row-up = "Up,Control+p,Control+k";
    };
  };
}
