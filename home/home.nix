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
    ./programs/alacritty.nix
    ./programs/direnv.nix
    ./programs/emacs.nix
    ./programs/fish.nix
    ./programs/git.nix
    ./programs/autorandr.nix
    ./programs/zathura.nix
    ./services/picom.nix
    ./services/redshift.nix
    ./services/dunst.nix
    ./services/misc.nix
    ./theme.nix
  ];

  home.packages = with pkgs; [
    # Panel
    xmobar

    # Utilities
    devour
    nitrogen
    xsecurelock
    lxsession
    xdotool
    xfce.xfce4-power-manager
    xarchiver
    qalculate-gtk
    unzip

    # Theming
    gnome.gnome-themes-extra
    gsettings-desktop-schemas

    # File managers
    pcmanfm
    ranger
    ueberzug

    # Dictionaries
    aspell
    aspellDicts.en
    aspellDicts.pl

    # Media
    feh
    gimp
    calibre
    libreoffice
    neofetch
    pulsemixer
    mpv

    # Doom emacs dependencies
    fd
    ripgrep
    libvterm

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
