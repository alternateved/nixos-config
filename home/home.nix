{ config, lib, pkgs, inputs, ... }:

{
  programs.home-manager.enable = true;
  home.username = "alternateved";
  home.homeDirectory = "/home/alternateved";
  home.sessionVariables = {
    ALTERNATE_EDITOR = "";
    EDITOR = "emacs -t -a ''";
    VISUAL = "emacs -c -a ''";
    BROWSER = "firefox-devedition";
  };

  imports = [
    ./modules/alacritty.nix
    ./modules/autorandr.nix
    ./modules/direnv.nix
    ./modules/dunst.nix
    ./modules/emacs.nix
    ./modules/fish.nix
    ./modules/git.nix
    ./modules/htop.nix
    ./modules/misc.nix
    ./modules/picom.nix
    ./modules/redshift.nix
    ./modules/starship.nix
    ./modules/theme.nix
    ./modules/zathura.nix
  ];

  home.packages = with pkgs; [
    # Panel
    xmobar

    # Utility
    devour
    nitrogen
    xsecurelock
    xdotool
    xarchiver
    qalculate-gtk
    unrar
    unzip

    # Theming
    gnome.gnome-themes-extra
    gsettings-desktop-schemas

    # File managers
    pcmanfm
    vifm
    ueberzug

    # Dictionaries
    aspell
    aspellDicts.en
    aspellDicts.pl

    # Media
    sxiv
    gimp
    calibre
    libreoffice
    neofetch
    pulsemixer
    mpv

    # Doom emacs dependencies
    fd
    ripgrep

    # Doom emacs module dependencies
    gnumake
    cmake
    gcc
    jq
    sbcl
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
    firefox-devedition-bin
    google-chrome-beta
    thunderbird
    signal-desktop
  ];

  home.stateVersion = "21.05";
}
