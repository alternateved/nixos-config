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
    ./nnn
    ./neovim
    ./picom
    ./polybar
    ./redshift
    ./starship/lambda.nix
    ./theme
    ./zathura
    ./zsh
  ];

  home.packages = with pkgs; [
    # Launcher
    dmenu
    # Utility
    devour
    exa
    neofetch
    unrar
    unzip
    qalculate-gtk
    xarchiver
    xwallpaper
    xsecurelock

    # Xorg
    wmctrl
    xdotool
    xclip
    killall
    xorg.xkill
    xorg.xwininfo

    # Theming
    gnome.gnome-themes-extra
    gsettings-desktop-schemas

    # File managers
    pcmanfm

    # Office
    aspell
    aspellDicts.en
    aspellDicts.pl
    libreoffice

    # Media
    sxiv
    calibre
    spotify
    pulsemixer
    youtube-dl

    # Doom dependencies
    fd
    ripgrep

    gnumake
    cmake
    gcc
    jq
    nixfmt
    sqlite
    graphviz
    pandoc
    python3Minimal
    shellcheck
    html-tidy
    nodePackages.stylelint
    nodePackages.js-beautify

    # Communication
    discord
    skypeforlinux
    thunderbird
    signal-desktop
  ];

  home.stateVersion = "21.05";
}
