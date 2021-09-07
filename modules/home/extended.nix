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
    # Utility
    devour
    exa
    neofetch
    unrar
    unzip
    xarchiver
    xwallpaper
    xsecurelock

    # Xorg
    xdotool
    xclip
    killall
    xorg.xkill

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
    playerctl
    pulseaudio
    pulsemixer
    youtube-dl

    # Doom dependencies
    fd
    ripgrep

    # Doom module dependencies
    gnumake
    cmake
    gcc
    jq
    graphviz
    pandoc
    ## org-roam
    sqlite
    ## treemacs
    python3Minimal
    ## Nix
    nixfmt
    ## sh
    shellcheck
    nodePackages.bash-language-server
    ## JS, TS, json and web
    nodejs
    html-tidy
    nodePackages.npm
    nodePackages.stylelint
    nodePackages.js-beautify
    nodePackages.typescript-language-server
    nodePackages.vscode-json-languageserver-bin
    nodePackages.vscode-html-languageserver-bin

    # Communication
    discord
    betterdiscordctl
    skypeforlinux
    thunderbird
    signal-desktop
  ];

  home.stateVersion = "21.05";
}
