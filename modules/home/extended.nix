{ config, lib, pkgs, inputs, ... }:

{
  programs.home-manager.enable = true;
  home.username = "alternateved";
  home.homeDirectory = "/home/alternateved";

  imports = [
    ./alacritty
    ./autorandr
    ./direnv
    ./dunst
    ./emacs
    ./firefox
    ./fzf
    ./git
    ./htop
    ./misc
    ./mpv
    ./ncspot
    ./nnn
    ./neovim
    ./picom
    ./redshift
    ./starship/lambda.nix
    ./theme
    ./zathura
    ./zsh
  ];

  home.packages = with pkgs; [
    # Archives
    unrar
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
    pamixer
    pulsemixer
    youtube-dl

    # Communication
    discord
    betterdiscordctl
    skypeforlinux
    thunderbird
    signal-desktop
  ];

  home.stateVersion = "21.05";
}
