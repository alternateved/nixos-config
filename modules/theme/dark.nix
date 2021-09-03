{ pkgs, ... }: {
  gtk = {
    theme = {
      package = pkgs.materia-theme;
      name = "Materia-dark-compact";
    };
    gtk3.extraConfig = { gtk-application-prefer-dark-theme = 1; };
    iconTheme = {
      package = pkgs.tela-icon-theme;
      name = "Tela";
    };
  };
}
