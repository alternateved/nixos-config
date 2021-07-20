{ config, lib, pkgs, inputs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.username = "alternateved";
  home.homeDirectory = "/home/alternateved";

  home.sessionVariables = {
    ALTERNATE_EDITOR = "";
    EDITOR = "emacs -t -a ''";
    VISUAL = "emacs -c -a ''";
    BROWSER = "firefox-devedition";
    CALIBRE_USE_DARK_PALETTE = 1;
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
        em = "devour emacsclient -c -a ''";
        mpv = "devour mpv";
        zathura = "devour zathura";
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

    autorandr = {
      enable = true;

      profiles = {
        "attached" = {
          fingerprint = {
            HDMI-1 =
              "00ffffffffffff001e6dfb5637d406000813010380331d780aaec5a2574a9c25125054a76b80b300818f8180714f01010101010101011a3680a070381f4030203500fe221100001e023a801871382d40532c4500fe221100001e000000fd00383d1e530f000a202020202020000000fc0057323336310a20202020202020014f020321f14e900403011412051f101300000000230907078301000065030c001000023a801871382d40582c4500fe221100001e011d8018711c1620582c2500fe221100009e011d007251d01e206e285500fe221100001e8c0ad08a20e02d10103e9600fe221100001800000000000000000000000000000000000000000000de";
            eDP-1 =
              "00ffffffffffff0009e55d060000000001190104952213780224109759548e271e5054000000010101010101010101010101010101013c3780de703814403020360058c21000001a000000000000000000000000000000000000000000fe00424f452048460a202020202020000000fe004e5631353646484d2d4e34320a006c";
          };
          config = {
            HDMI-1 = {
              enable = true;
              primary = true;
              position = "0x0";
              mode = "1920x1080";
              rotate = "normal";
            };
            eDP-1 = {
              enable = true;
              primary = false;
              position = "1920x360";
              mode = "1368x768";
              rotate = "normal";
            };
          };
        };
        "detached" = {
          fingerprint = {
            eDP-1 =
              "00ffffffffffff0009e55d060000000001190104952213780224109759548e271e5054000000010101010101010101010101010101013c3780de703814403020360058c21000001a000000000000000000000000000000000000000000fe00424f452048460a202020202020000000fe004e5631353646484d2d4e34320a006c";
          };
          config = {
            HDMI-1 = { enable = false; };
            eDP-1 = {
              enable = true;
              primary = true;
              position = "0x0";
              mode = "1920x1080";
              rotate = "normal";
            };
          };
        };
      };
    };

    zathura = {
      enable = true;
      options = {
        font = "JetBrains Mono Nerd Font 10";
        recolor = true;
        default-bg = "#1d1f21";
        default-fg = "#c4c8c5";
        statusbar-bg = "#545B68";
        statusbar-fg = "#c4c8c5";
        inputbar-bg = "#1d1f21";
        inputbar-fg = "#c4c8c5";
        notification-error-bg = "#1d1f21";
        notification-error-fg = "#cc6666";
        notification-warning-bg = "#1d1f21";
        notification-warning-fg = "#e7c787";
        highlight-color = "#d5c4a1";
        highlight-active-color = "#c4c8c5";
        completion-highlight-bg = "#c4c8c5";
        completion-highlight-fg = "#1d1f21";
        completion-bg = "#1d1f21";
        completion-fg = "#c4c8c5";
        notification-fg = "#c4c8c5";
        recolor-lightcolor = "#1d1f21";
        recolor-darkcolor = "#c4c8c5";
        selection-clipboard = "clipboard";
        window-title-basename = true;
        statusbar-h-padding = 10;
        statusbar-v-padding = 4;

      };

      extraConfig = ''
        map a adjust_window best-fit
        map s adjust_window width
        map f follow
        map <Tab> toggle_index
        map j scroll down
        map k scroll up
        map h navigate previous
        map l navigate next
        map <C-r> recolor
      '';
    };
  };

  services = {
    emacs.package = pkgs.emacsUnstable;
    flameshot.enable = true;

    picom = {
      enable = true;
      vSync = true;
      fade = false;
      shadow = false;
      blur = true;
      shadowExclude = [ "class_g = 'Firefox Developer Edition' && argb" ];
      blurExclude = [ "class_g = 'Firefox Developer Edition' && argb" ];
      backend = "glx";
      experimentalBackends = true;
      extraOptions = ''
        use-damage = true;
        blur-strength = 12;
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
    dunst = {
      enable = true;
      iconTheme = {
        name = "Papirus-Dark";
        package = pkgs.papirus-icon-theme;
        size = "22x22";
      };
      settings = {
        global = {
          monitor = 0;
          follow = "mouse";
          geometry = "380x20-10+28";
          alignment = "left";
          bounce_freq = 0;
          corner_radius = 2;
          font = "Overpass 11";
          format = "%b";
          frame_width = 1;
          history_length = 20;
          horizontal_padding = 16;
          icon_position = "left";
          idle_threshold = 120;
          ignore_newline = "no";
          indicate_hidden = "yes";
          line_height = 0;
          markup = "full";
          max_icon_size = 48;
          padding = 20;
          separator_color = "auto";
          separator_height = 4;
          show_age_threshold = 60;
          show_indicators = "false";
          shrink = "no";
          sort = "yes";
          startup_notification = false;
          sticky_history = "yes";
          transparency = 5;
          word_wrap = "yes";
        };
        shortcuts = {
          close = "ctrl+shift+space";
          close_all = "ctrl+shift+space";
          history = "ctrl+shift+comma";
          context = "ctrl+shift+period";
        };
        urgency_low = {
          background = "#27292b";
          foreground = "#ecf0ed";
          frame_color = "#313335";
          timeout = 8;
        };
        urgency_normal = {
          background = "#27292b";
          foreground = "#ecf0ed";
          frame_color = "#313335";
          timeout = 14;
        };
        urgency_critical = {
          background = "#cc6666";
          foreground = "#1E2029";
          frame_color = "#313335";
          timeout = 0;
        };
      };

    };
  };

  dconf.enable = false;

  xdg = {
    enable = true;
    userDirs.enable = true;
  };

  gtk = {
    enable = true;

    theme = {
      package = pkgs.materia-theme;
      name = "Materia-dark-compact";
    };

    gtk3.extraConfig = { gtk-application-prefer-dark-theme = 1; };

    iconTheme = {
      package = pkgs.papirus-icon-theme;
      name = "Papirus-Dark";
    };

    font = {
      package = pkgs.overpass;
      name = "Overpass Regular 11";
    };
  };

  qt = {
    enable = true;
    platformTheme = "gtk";
  };

  xsession.pointerCursor = {
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ";
    size = 16;
  };

  home.packages = with pkgs; [
    # Terminal
    alacritty

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

    # Theming
    gnome.gnome-themes-extra
    gsettings-desktop-schemas

    # File managers
    pcmanfm
    ranger

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
    jq
    nixfmt
    pandoc
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
