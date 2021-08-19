{ config, lib, pkgs, inputs, ... }:

{
  programs.home-manager.enable = true;
  home.username = "alternateved";
  home.homeDirectory = "/home/alternateved";

  imports = [
    ./autorandr
    ./direnv
    ./dunst
    ./emacs
    ./fzf
    ./git
    ./htop
    ./kitty
    ./misc
    ./mpv
    ./neovim
    ./picom
    ./polybar
    ./qutebrowser
    ./redshift
    ./starship/lambda.nix
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
    nnn

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
    pulsemixer

    # Doom dependencies
    fd
    ripgrep

    # Doom emacs module dependencies
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

    # Surfing
    firefox-devedition-bin
    google-chrome-beta
    youtube-dl

    # Communication
    discord
    skypeforlinux
    thunderbird
    signal-desktop
  ];

  home.stateVersion = "21.05";
}
