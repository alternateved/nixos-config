{ pkgs, ... }: {
  gtk = {
    theme = {
      package = pkgs.materia-theme;
      name = "Materia-light-compact";
    };
    gtk3.extraConfig = { gtk-application-prefer-dark-theme = 0; };
    iconTheme = {
      package = pkgs.tela-icon-theme;
      name = "Tela";
    };
    font = {
      package = null;
      name = "Iosevka Aile Medium 11";
    };
  };
}
