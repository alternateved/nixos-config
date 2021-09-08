{ pkgs, ... }: {
  imports = [
    # ./xresources.nix
    ./light.nix
    #     ./dark.nix
  ];
  dconf.enable = true;
  gtk = {
    enable = true;
    font = {
      package = null;
      name = "Iosevka Aile Medium 11";
    };
  };
  qt = {
    enable = true;
    platformTheme = "gtk";
  };

  home.packages = with pkgs; [
    gnome.gnome-themes-extra
    gsettings-desktop-schemas
    gnome3.dconf
    libsForQt5.qtstyleplugins
  ];

  xsession.pointerCursor = {
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ";
    size = 16;
  };
  fonts.fontconfig.enable = true;
}
