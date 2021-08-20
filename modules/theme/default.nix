{ pkgs, ... }: {
  imports = [ ./xresources.nix ];
  dconf.enable = false;
  gtk = {
    enable = true;
    theme = {
      package = pkgs.materia-theme;
      name = "Materia-dark-compact";
    };
    gtk3.extraConfig = { gtk-application-prefer-dark-theme = 1; };
    iconTheme = {
      package = pkgs.tela-icon-theme;
      name = "Tela black dark";
    };
    font = {
      package = null;
      name = "Iosevka Aile Medium 11";
    };
  };
  qt = {
    enable = true;
    platformTheme = "gtk";
  };

  home.packages = with pkgs; [ libsForQt5.qtstyleplugins ];
  xsession.pointerCursor = {
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ";
    size = 16;
  };
  fonts.fontconfig.enable = true;
}
