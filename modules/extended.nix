{ config, lib, pkgs, inputs, ... }: {
  programs.home-manager.enable = true;
  home.username = "alternateved";
  home.homeDirectory = "/home/alternateved";

  home.packages = with pkgs; [
    # Archives
    unrar
    zip
    unzip
    xarchiver

    # File managers
    pcmanfm

    # Office
    libreoffice

    # Media
    sxiv
    calibre
    spotify
    playerctl
    alsa-utils
    pamixer
    pulsemixer
    youtube-dl

    # Communication
    discord
    betterdiscordctl
    skypeforlinux
    signal-desktop

    # Other
    google-chrome
  ];

  home.stateVersion = "21.05";
}
