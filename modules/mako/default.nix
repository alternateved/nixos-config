let colors = import ../theme/colors.nix;
in {
  home-manager.users.alternateved.programs.mako = {
    enable = true;
    font = "Iosevka Aile Medium 11";
    backgroundColor = colors.background;
    textColor = colors.foreground;
    width = 400;
    height = 100;
    padding = "10";
    borderSize = 0;
    borderRadius = 5;
  };
}
