let colors = import ../theme/colors.nix;
in {
  home-manager.users.alternateved.programs.zathura = {
    enable = true;
    options = {
      font = "Rec Mono Linear 10";
      recolor = true;
      default-bg = colors.background;
      default-fg = colors.foreground;
      statusbar-bg = colors.black;
      statusbar-fg = colors.foreground;
      inputbar-bg = colors.background;
      inputbar-fg = colors.foreground;
      notification-error-bg = colors.background;
      notification-error-fg = colors.red;
      notification-warning-bg = colors.background;
      notification-warning-fg = colors.bright-yellow;
      highlight-color = colors.yellow;
      highlight-active-color = colors.white;
      completion-highlight-bg = colors.foreground;
      completion-highlight-fg = colors.background;
      completion-bg = colors.background;
      completion-fg = colors.foreground;
      notification-fg = colors.foreground;
      recolor-lightcolor = colors.background;
      recolor-darkcolor = colors.foreground;
      selection-clipboard = "clipboard";
      window-title-basename = true;
      statusbar-h-padding = 10;
      statusbar-v-padding = 4;
    };
    extraConfig = ''
      map a adjust_window best-fit
      map s adjust_window width
      map f follow
      map <Tab> toggle_index
      map j scroll down
      map k scroll up
      map h navigate previous
      map l navigate next
      map <C-r> recolor
    '';
  };
}
