{ pkgs, ... }:

let colors = import ./colors.nix;
in {
  dconf.enable = false;
  xdg = {
    enable = true;
    userDirs.enable = true;
  };
  gtk = {
    enable = true;
    theme = {
      package = pkgs.materia-theme;
      name = "Materia-dark-compact";
    };
    gtk3.extraConfig = { gtk-application-prefer-dark-theme = 1; };
    iconTheme = {
      package = pkgs.papirus-icon-theme;
      name = "Papirus-Dark";
    };
    font = {
      package = null;
      name = "Iosevka Aile Semibold 11";
    };
  };
  qt = {
    enable = true;
    platformTheme = "gtk";
  };
  xresources.extraConfig = ''
    Sxiv.background: ${colors.background}
    Sxiv.foreground: ${colors.foreground}
    Sxiv.font: Iosevka Nerd Font Mono:style=regular:size=11
  '';
  xsession.pointerCursor = {
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ";
    size = 16;
  };
  home.packages = with pkgs; [ libsForQt5.qtstyleplugins ];
}
