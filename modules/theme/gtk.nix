{ pkgs, ... }: {
  dconf.enable = false;
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
      name = "Iosevka Aile Medium 11";
    };
  };
}
