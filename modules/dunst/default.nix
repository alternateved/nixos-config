{ pkgs, ... }:

let colors = import ../theme/colors.nix;
in {
  home-manager.users.alternateved.services.dunst = {
    enable = true;
    iconTheme = {
      name = "Tela";
      package = pkgs.tela-icon-theme;
      size = "22x22";
    };
    settings = {
      global = {
        monitor = 0;
        follow = "mouse";
        geometry = "380x20-18+36";
        alignment = "left";
        bounce_freq = 0;
        corner_radius = 2;
        font = "CMU Sans Serif 13";
        format = "%b";
        frame_width = 0;
        history_length = 20;
        horizontal_padding = 16;
        icon_position = "left";
        idle_threshold = 120;
        ignore_newline = "no";
        indicate_hidden = "yes";
        line_height = 0;
        markup = "full";
        max_icon_size = 48;
        padding = 20;
        separator_color = "auto";
        separator_height = 4;
        show_age_threshold = 60;
        show_indicators = "false";
        shrink = "no";
        sort = "yes";
        startup_notification = false;
        sticky_history = "yes";
        transparency = 5;
        word_wrap = "yes";
      };
      shortcuts = {
        close = "ctrl+shift+space";
        close_all = "ctrl+shift+space";
        history = "ctrl+shift+comma";
        context = "ctrl+shift+period";
      };
      urgency_low = {
        background = colors.background;
        foreground = colors.foreground;
        frame_color = colors.white;
        timeout = 8;
      };
      urgency_normal = {
        background = colors.background;
        foreground = colors.foreground;
        frame_color = colors.white;
        timeout = 14;
      };
      urgency_critical = {
        background = colors.red;
        foreground = colors.white;
        frame_color = colors.white;
        timeout = 0;
      };

      fullscreen_pushback_everything = { fullscreen = "pushback"; };
    };
  };
}
