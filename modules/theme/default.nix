{ pkgs, ... }: {
  imports = [
    # ./xresources.nix
    ./light.nix
#    ./dark.nix
  ];
  home-manager.users.alternateved.dconf.enable = true;
  home-manager.users.alternateved.gtk = {
    enable = true;
    font = {
      package = null;
      name = "Iosevka Aile Medium 11";
    };
  };
  home-manager.users.alternateved.qt = {
    enable = true;
    platformTheme = "gtk";
  };

  home-manager.users.alternateved.xsession.pointerCursor = {
    package = pkgs.capitaine-cursors;
    name = "capitaine-cursors-white";
    size = 16;
  };

  home-manager.users.alternateved.home.packages = with pkgs; [
    gnome.gnome-themes-extra
    gsettings-desktop-schemas
    gnome3.dconf
    libsForQt5.qtstyleplugins
  ];
}
