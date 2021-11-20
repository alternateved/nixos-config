{ config, lib, pkgs, ... }:

let colors = import ../theme/colors.nix;
in {
  home-manager.users.alternateved.programs.kitty = {
    enable = true;
    font = {
      name = "Iosevka Nerd Font Complete";
      size = 13;
    };
    settings = {
      shell = "zsh";
      window_padding_width = 5;
      adjust_line_height = 1;
      enable_audio_bell = "no";
      placement_strategy = "center";
      default_pointer_shape = "beam";
      cursor_shape = "block";
      tab_bar_style = "fade";
      tab_fade = "1 1 1";

      # for nnn image preview
      allow_remote_control = "yes";
      listen_on = "unix:/tmp/kitty";

      active_tab_foreground = "${colors.background}";
      active_tab_background = "${colors.foreground}";
      inactive_tab_foreground = "${colors.background}";
      inactive_tab_background = "${colors.bright-black}";

      foreground = "${colors.foreground}";
      background = "${colors.background}";

      cursor = "${colors.foreground}";
      cursor_text_color = "${colors.background}";

      color0 = "${colors.black}";
      color1 = "${colors.red}";
      color2 = "${colors.green}";
      color3 = "${colors.yellow}";
      color4 = "${colors.blue}";
      color5 = "${colors.magenta}";
      color6 = "${colors.cyan}";
      color7 = "${colors.white}";
      color8 = "${colors.bright-black}";
      color9 = "${colors.bright-red}";
      color10 = "${colors.bright-green}";
      color11 = "${colors.bright-yellow}";
      color12 = "${colors.bright-blue}";
      color13 = "${colors.bright-magenta}";
      color14 = "${colors.bright-cyan}";
      color15 = "${colors.bright-white}";
    };
  };
}
