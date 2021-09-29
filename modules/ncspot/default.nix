let colors = import ../theme/colors.nix;
in {
  home-manager.users.alternateved.programs.ncspot = {
    enable = true;
    settings = {
      use_nerdfont = true;
      gapless = true;
      theme = {
        background = colors.background;
        primary = colors.foreground;
        secondary = colors.bright-black;
        title = colors.cyan;
        playing = colors.cyan;
        playing_selected = colors.cyan;
        playing_bg = colors.background;
        highlight = colors.cyan;
        highlight_bg = colors.background;
        error = colors.foreground;
        error_bg = colors.magenta;
        statusbar = colors.background;
        statusbar_progress = colors.cyan;
        statusbar_bg = colors.cyan;
        cmdline = colors.bright-black;
        cmdline_bg = colors.background;
      };

    };

  };
}
