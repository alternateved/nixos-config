{ pkgs, ... }:
let colors = import ../theme/colors.nix;
in {
  home-manager.users.alternateved.programs.mako = {
    enable = true;
    font = "Iosevka Aile Medium 11";
    backgroundColor = colors.background;
    textColor = colors.foreground;
    maxVisible = 3;
    width = 400;
    height = 100;
    padding = "10";
    borderSize = 0;
    defaultTimeout = 5000;
    extraConfig = ''
      [mode=do-not-disturb]
      invisible=1
    '';
  };
  home-manager.users.alternateved.home.packages = with pkgs; [ libnotify ];
}
