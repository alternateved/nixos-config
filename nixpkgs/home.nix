{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.username = "alternateved";
  home.homeDirectory = "/home/alternateved";

  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];

  home.sessionVariables = {
    ALTERNATE_EDITOR = "";
    EDITOR = "emacs -t -a ''";
    VISUAL = "emacs -c -a ''";
    BROWSER = "firefox-devedition";
  };

  programs = {

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    alacritty = {
      enable = true;
      settings = {

        background_opacity = 1.0;

        window = { decorations = "none"; };
        font = {
          size = 9.0;
          normal.family = "JetBrains Mono Nerd Font";
          bold.family = "JetBrains Mono Nerd Font";
          italic.family = "JetBrains Mono Nerd Font";
        };

        cursor.style = {
          shape = "Underline";
          blinking = "On";
        };

        colors = {
          primary = {
            background = "#1d1f21";
            foreground = "#c5c8c6";
          };
        };

        mouse.hide_when_typing = true;
        key_bindings = [

          {
            key = "V";
            mods = "Control|Shift";
            action = "Paste";
          }
          {
            key = "C";
            mods = "Control|Shift";
            action = "Copy";
          }
          {
            key = "Insert";
            mods = "Shift";
            action = "PasteSelection";
          }
          {
            key = "Key0";
            mods = "Control";
            action = "ResetFontSize";
          }
          {
            key = "Equals";
            mods = "Control";
            action = "IncreaseFontSize";
          }
          {
            key = "Plus";
            mods = "Control";
            action = "IncreaseFontSize";
          }
          {
            key = "NumpadAdd";
            mods = "Control";
            action = "IncreaseFontSize";
          }
          {
            key = "Minus";
            mods = "Control";
            action = "DecreaseFontSize";
          }
          {
            key = "NumpadSubtract";
            mods = "Control";
            action = "DecreaseFontSize";
          }
          {
            key = "F11";
            mods = "None";
            action = "ToggleFullscreen";
          }
          {
            key = "Paste";
            mods = "None";
            action = "Paste";
          }
          {
            key = "Copy";
            mods = "None";
            action = "Copy";
          }
          {
            key = "L";
            mods = "Control";
            action = "ClearLogNotice";
          }
          {
            key = "L";
            mods = "Control";
            chars = "x0c";
          }
          {
            key = "PageUp";
            mods = "None";
            action = "ScrollPageUp";
            mode = "~Alt";
          }
          {
            key = "K";
            mods = "Control|Shift";
            action = "ScrollPageUp";
            mode = "~Alt";
          }
          {
            key = "J";
            mods = "Control|Shift";
            action = "ScrollPageDown";
            mode = "~Alt";
          }
          {
            key = "PageDown";
            mods = "None";
            action = "ScrollPageDown";
            mode = "~Alt";
          }
          {
            key = "Home";
            mods = "Shift";
            action = "ScrollToTop";
            mode = "~Alt";
          }
          {
            key = "End";
            mods = "Shift";
            action = "ScrollToBottom";
            mode = "~Alt";
          }

        ];
      };
    };

    fish = {
      enable = true;
      shellAliases = {
        cp = "cp -i"; # Confirm before overwriting something
        df = "df -h"; # Human-readable sizes
        free = "free -m"; # Show sizes in MB
        ls = "ls --color=auto"; # colored output
      };
    };

    starship = {
      enable = true;
      enableFishIntegration = true;
    };

    git = {
      enable = true;
      userName = "alternateved";
      userEmail = "alternateved@gmail.com";
    };

    emacs = {
      enable = true;
      package = pkgs.emacsGcc;
    };
  };

  services = {
    emacs.package = pkgs.emacsUnstable;
    flameshot.enable = true;
    dunst.iconTheme = "Papirus-Dark";

    picom = {
      enable = true;
      shadow = false;
      shadowExclude = [ "class_g = 'Firefox Developer Edition' && argb" ];
      fade = false;
      blur = true;
      blurExclude = [ "class_g = 'Firefox Developer Edition' && argb" ];
      backend = "glx";
      vSync = true;
      extraOptions = ''
        glx-no-rebind-pixmap  = true;
        glx-no-stencil        = true;
        use-damage            = true;
      '';
    };

    redshift = {
      enable = true;
      provider = "manual";
      latitude = 54.7;
      longitude = 19.4;
      temperature = {
        day = 5700;
        night = 3000;
      };
    };

  };
  dconf.enable = true;

  xdg = {
    enable = true;
    userDirs.enable = true;
  };

  qt = {
    enable = true;
    platformTheme = "gtk"; # gnome or gtk
  };

  home.packages = with pkgs; [
    # Terminal
    alacritty

    # temporary haskell stuff
    xmobar

    # utilities
    dunst
    nitrogen
    slock
    lxsession
    xfce.xfce4-power-manager
    xarchiver
    qalculate-gtk

    # Theming
    lxappearance
    materia-theme
    papirus-icon-theme
    gtk-engine-murrine
    gsettings-desktop-schemas
    vanilla-dmz

    # File managers
    pcmanfm
    ranger

    # Dictionaries
    aspell
    aspellDicts.en
    aspellDicts.pl

    # Media
    gimp
    zathura
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
    hlint
    cabal-install
    jq
    nixfmt
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
