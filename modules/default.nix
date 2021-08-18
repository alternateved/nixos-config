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
    ./git
    ./htop
    ./misc
    ./mpv
    ./neovim
    ./picom
    ./polybar
    ./redshift
    ./starship
    ./theme
    ./zathura
    ./zsh
  ];

  home.packages = with pkgs; [
    # xmobar
    # Launcher
    (dmenu.overrideAttrs (old: rec {
      patches = [
        (fetchpatch {
          url =
            "https://tools.suckless.org/dmenu/patches/password/dmenu-password-5.0.diff";
          sha256 = "1dqxiwwwbya9slm3xbbal564rnigfbr497kac9pxlikjqgpz9a1q";
        })
      ];
    }))

    # Utility
    bitwarden-cli
    devour
    xwallpaper
    xsecurelock
    xarchiver
    qalculate-gtk
    unrar
    unzip
    exa

    # Xorg
    wmctrl
    xclip
    xdotool
    killall
    xorg.xkill
    xorg.xwininfo

    # Theming
    gnome.gnome-themes-extra
    gsettings-desktop-schemas

    # File managers
    pcmanfm
    nnn
    ueberzug

    # Dictionaries
    aspell
    aspellDicts.en
    aspellDicts.pl

    # Media
    sxiv
    gimp
    calibre
    spotify
    libreoffice
    neofetch
    pulsemixer

    # Doom emacs dependencies
    fd
    ripgrep

    # Doom emacs module dependencies
    gnumake
    cmake
    gcc
    jq
    rnix-lsp
    nixfmt
    sqlite
    graphviz
    pandoc
    python3Minimal
    shellcheck
    html-tidy
    nodePackages.stylelint
    nodePackages.js-beautify

    # Surfing
    firefox-devedition-bin
    google-chrome-beta
    qutebrowser
    youtube-dl

    # Communication
    discord
    skypeforlinux
    thunderbird
    signal-desktop
  ];

  home.stateVersion = "21.05";
}
