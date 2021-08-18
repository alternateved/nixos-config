{ config, lib, pkgs, inputs, ... }:

{
  programs.home-manager.enable = true;
  home.username = "alternateved";
  home.homeDirectory = "/home/alternateved";

  imports = [
    ./modules/alacritty.nix
    ./modules/autorandr.nix
    ./modules/direnv.nix
    ./modules/dunst.nix
    ./modules/emacs.nix
    ./modules/git.nix
    ./modules/htop.nix
    ./modules/misc.nix
    ./modules/mpv.nix
    ./modules/neovim.nix
    ./modules/picom.nix
    ./modules/polybar.nix
    ./modules/redshift.nix
    ./modules/starship.nix
    ./modules/theme.nix
    ./modules/xresources.nix
    ./modules/zathura.nix
    ./modules/zsh.nix
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
