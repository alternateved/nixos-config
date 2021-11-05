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
    nyxt
    k380-function-keys-conf
    ungoogled-chromium
    android-tools
  ];

  home.stateVersion = "21.11";
}
