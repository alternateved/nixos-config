{ config, lib, pkgs, inputs, ... }: {
  programs.home-manager.enable = true;
  home.username = "alternateved";
  home.homeDirectory = "/home/alternateved";

  home.packages = with pkgs; [
    # Archives
    unrar
    unzip
    zip

    # File manager
    xfce.thunar
    xfce.thunar-volman
    xfce.thunar-archive-plugin


    # Office
    libreoffice

    # Media
    sxiv
    calibre
    spotify
    playerctl
    pamixer
    pulsemixer

    # Communication
    slack
    discord
    skypeforlinux
    signal-desktop

    # Other
    k380-function-keys-conf
    ungoogled-chromium
  ];

  home.stateVersion = "21.11";
}
