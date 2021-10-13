{ pkgs, ... }:
let theme = import ../theme/colors.nix;
in {
  imports = [
    ../alacritty
    ../direnv
    ../emacs
    ../firefox
    ../fzf
    ../git
    ../gammastep
    ../htop
    ../mail
    ../mako
    ../mpv
    ../ncspot
    ../nnn
    ../neovim
    ../starship/lambda.nix
    ../theme
    ../xdg
    ../zathura
    ../zsh
  ];

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd sway";
      };
      initial_session = {
        command = "sway";
        user = "alternateved";
      };
    };
  };

  programs.sway = {
    enable = true;
    extraPackages = with pkgs; [
      swayidle
      swaylock
      xwayland
      mako
      wl-clipboard
      gammastep
      bemenu
      grim
      slurp
      sway-contrib.grimshot
      i3status-rust
      light
    ];
  };

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-wlr xdg-desktop-portal-gtk ];
    gtkUsePortal = true;
  };

  home-manager.users.alternateved.wayland.windowManager.sway = let
    gsettings = "${pkgs.glib}/bin/gsettings";
    gnomeSchema = "org.gnome.desktop.interface";
    importGsettings = pkgs.writeShellScript "import_gsettings.sh" ''
      config="/home/alternateved/.config/gtk-3.0/settings.ini"
      if [ ! -f "$config" ]; then exit 1; fi
      gtk_theme="$(grep 'gtk-theme-name' "$config" | sed 's/.*\s*=\s*//')"
      icon_theme="$(grep 'gtk-icon-theme-name' "$config" | sed 's/.*\s*=\s*//')"
      cursor_theme="$(grep 'gtk-cursor-theme-name' "$config" | sed 's/.*\s*=\s*//')"
      font_name="$(grep 'gtk-font-name' "$config" | sed 's/.*\s*=\s*//')"
      ${gsettings} set ${gnomeSchema} gtk-theme "$gtk_theme"
      ${gsettings} set ${gnomeSchema} icon-theme "$icon_theme"
      ${gsettings} set ${gnomeSchema} cursor-theme "$cursor_theme"
      ${gsettings} set ${gnomeSchema} font-name "$font_name"
    '';
  in {
    enable = true;
    xwayland = true;
    systemdIntegration = true;
    wrapperFeatures = {
      base = true;
      gtk = true;
    };

    config = rec {
      modifier = "Mod4";

      input = {
        "type:touchpad" = {
          tap = "enabled";
          dwt = "enabled";
          middle_emulation = "enabled";
          scroll_method = "two_finger";
          natural_scroll = "enabled";
        };
        "type:keyboard" = {
          xkb_layout = "pl";
          xkb_options = "caps:escape_shifted_capslock";
        };
      };

      output = {
        "*".bg = "~/.cache/current.png fill";
        "*".scale = "1";
      };

      seat = {
        seat0 = { xcursor_theme = "capitaine-cursors-white 16"; };
        "*".hide_cursor = "when-typing enable";
      };

      fonts = {
        names = [ "Iosevka Nerd Font" ];
        size = 11.0;
      };

      focus = { followMouse = "always"; };

      gaps = {
        inner = 5;
        outer = 5;
        smartGaps = true;
        smartBorders = "on";
      };

      window = {
        border = 2;
        titlebar = false;
        commands = [
          {
            criteria = { title = "^(.*) Indicator"; };
            command = "floating enable";
          }
          {
            criteria = { class = "Sxiv"; };
            command = "floating enable";
          }
          {
            criteria = { class = "Spotify"; };
            command = "move window to workspace 5";
          }
          {
            criteria = { app_id = "terrm"; };
            command =
              "floating enable, resize set width 900px 600px, move scratchpad, scratchpad show";
          }
          {
            criteria = { app_id = "haskell_repl"; };
            command =
              "floating enable, resize set width 900px 700px, move scratchpad, scratchpad show";
          }
          {
            criteria = { app_id = "mixerr"; };
            command =
              "floating enable, resize set width 1100px 350px, move scratchpad, scratchpad show";
          }
          {
            criteria = { app_id = "monitorr"; };
            command =
              "floating enable, resize set width 1400px 800px, move scratchpad, scratchpad show";
          }
          {
            criteria = { app_id = "spotiff"; };
            command =
              "floating enable, resize set width 1200px 600px, move scratchpad, scratchpad show";
          }
          {
            criteria = { title = "scratcher"; };
            command = "floating enable, move scratchpad, scratchpad show";
          }
        ];
      };

      startup = [
        { command = "${importGsettings}"; }
        { command = "bluetoothctl power on"; }
        { command = "${pkgs.xorg.xrdb}/bin/xrdb -merge ~/.Xresources"; }
        {
          always = true;
          command = ''
            swayidle -w timeout 600 'swaymsg "output * dpms off"' resume 'swaymsg "output * dpms on"' '';
        }
      ];

      assigns = {
        "1" = [{ class = "Firefox"; }];
        "2" = [ { class = "discord"; } { class = "Signal"; } ];
        "5" = [{ app_id = "mpv"; }];
      };

      colors = rec {
        background = theme.background;
        unfocused = {
          text = theme.foreground;
          border = theme.foreground;
          background = theme.background;
          childBorder = theme.background;
          indicator = theme.foreground;
        };
        focusedInactive = unfocused;
        urgent = unfocused // {
          text = theme.foreground;
          border = theme.red;
          childBorder = theme.red;
        };
        focused = unfocused // {
          childBorder = theme.foreground;
          border = theme.foreground;
          background = theme.foreground;
          text = theme.background;
        };
      };

      bars = [{
        fonts = {
          names = [ "Iosevka Nerd Font" ];
          size = 11.0;
        };
        colors = {
          background = theme.background;
          statusline = theme.foreground;
          separator = theme.foreground;
          focusedWorkspace = {
            border = theme.foreground;
            background = theme.foreground;
            text = theme.background;
          };

          inactiveWorkspace = {
            border = theme.background;
            background = theme.background;
            text = theme.foreground;
          };

          urgentWorkspace = {
            border = theme.background;
            background = theme.background;
            text = theme.red;
          };

          bindingMode = {
            border = theme.cyan;
            background = theme.cyan;
            text = theme.background;
          };

        };
        mode = "dock";
        position = "top";
        trayOutput = "none";
        statusCommand =
          "${pkgs.i3status-rust}/bin/i3status-rs ~/.config/i3status-rust/config-top.toml";
      }];

      modes = {
        resize = {
          h = "resize shrink width 10 px or 10 ppt";
          l = "resize grow width 10 px or 10 ppt";
          k = "resize shrink height 10 px or 10 ppt";
          j = "resize grow height 10 px or 10 ppt";
          Left = "resize shrink width 10 px or 10 ppt";
          Right = "resize grow width 10 px or 10 ppt";
          Up = "resize shrink height 10 px or 10 ppt";
          Down = "resize grow height 10 px or 10 ppt";
          Return = "mode default";
          Escape = "mode default";
        };
        scratchpad = {
          t = ''
            exec swaymsg "[app_id=terrm] scratchpad show " || exec ${terminal} --class terrm, mode default'';
          h = ''
            exec swaymsg "[app_id=haskell_repl] scratchpad show " || exec ${terminal} --class haskell_repl --working-directory /home/alternateved/Documents/Programming/haskell/learn4haskell -e nix-shell --pure --run ghci, mode default'';
          v = ''
            exec swaymsg "[app_id=mixerr] scratchpad show " || exec ${terminal} --class mixerr -e pulsemixer, mode default'';
          m = ''
            exec swaymsg "[app_id=monitorr] scratchpad show " || exec ${terminal} --class monitorr -e htop, mode default'';
          s = ''
            exec swaymsg "[app_id=spotiff] scratchpad show " || exec ${terminal} --class spotiff -e ncspot, mode default'';
          e =
            "exec swaymsg \"[title=scratcher] scratchpad show \" || exec emacsclient -a '' --eval \"(open-scratch-frame)\", mode default";
          Return = ''mode "default"'';
          Escape = ''mode "default"'';
        };
        workspace = {
          a =
            "exec bash ~/.nixos-config/config/sway/add_workspace, mode default";
          r =
            "exec bash ~/.nixos-config/config/sway/rename_workspace, mode default";
          s =
            "exec bash ~/.nixos-config/config/sway/switch_workspace, mode default";
          Return = ''mode "default"'';
          Escape = ''mode "default"'';
        };
        chat = {
          s =
            "exec signal-desktop --enable-features=UseOzonePlatform --ozone-platform=wayland, mode default";
          d =
            "exec discord --enable-features=UseOzonePlatform --ozone-platform=wayland, mode default";
          Return = ''mode "default"'';
          Escape = ''mode "default"'';
        };
      };

      terminal = "${pkgs.alacritty}/bin/alacritty";
      menu = "exec bash " + ../../config/scripts/menu;

      keybindings = let
        screenshot_dir =
          "Pictures/Screenshots/$(date +'%Y-%m-%d+%H:%M:%S').png";
      in {
        "${modifier}+Shift+Return" = "exec ${terminal}";
        "${modifier}+Shift+c" = "kill";
        "${modifier}+Shift+r" = "reload";

        "${modifier}+p" = "${menu}";

        "${modifier}+Left" = "focus left";
        "${modifier}+Down" = "focus down";
        "${modifier}+Up" = "focus up";
        "${modifier}+Right" = "focus right";

        "${modifier}+Shift+Left" = "move left";
        "${modifier}+Shift+Down" = "move down";
        "${modifier}+Shift+Up" = "move up";
        "${modifier}+Shift+Right" = "move right";

        "${modifier}+Control+Left" = "resize shrink width 20 px";
        "${modifier}+Control+Down" = "resize grow height 20 px";
        "${modifier}+Control+Up" = "resize shrink height 20 px";
        "${modifier}+Control+Right" = "resize grow width 20 px";

        "${modifier}+h" = "focus left";
        "${modifier}+j" = "focus down";
        "${modifier}+k" = "focus up";
        "${modifier}+l" = "focus right";

        "${modifier}+Shift+h" = "move left";
        "${modifier}+Shift+j" = "move down";
        "${modifier}+Shift+k" = "move up";
        "${modifier}+Shift+l" = "move right";

        "${modifier}+Control+h" = "resize shrink width 20 px";
        "${modifier}+Control+j" = "resize grow height 20 px";
        "${modifier}+Control+k" = "resize shrink height 20 px";
        "${modifier}+Control+l" = "resize grow width 20 px";

        "${modifier}+Shift+Tab" = "workspace prev";
        "${modifier}+Tab" = "workspace next";

        "${modifier}+c" = "split v";
        "${modifier}+v" = "split h";

        "${modifier}+f" = "fullscreen toggle";

        "${modifier}+a" = "focus parent";
        "${modifier}+d" = "focus child";
        "${modifier}+n" = "focus next";
        "${modifier}+Shift+n" = "focus prev";

        "${modifier}+q" = "layout stacking";
        "${modifier}+w" = "layout tabbed";
        "${modifier}+e" = "layout toggle split";

        "${modifier}+t" = "floating toggle";
        "${modifier}+Shift+t" = "sticky toggle";
        "${modifier}+space" = "focus mode_toggle";

        "${modifier}+b" = "bar mode toggle";

        "${modifier}+1" = "workspace number 1";
        "${modifier}+2" = "workspace number 2";
        "${modifier}+3" = "workspace number 3";
        "${modifier}+4" = "workspace number 4";
        "${modifier}+5" = "workspace number 5";
        "${modifier}+6" = "workspace number 6";
        "${modifier}+7" = "workspace number 7";
        "${modifier}+8" = "workspace number 8";
        "${modifier}+9" = "workspace number 9";

        "${modifier}+Shift+1" = "move container to workspace number 1";
        "${modifier}+Shift+2" = "move container to workspace number 2";
        "${modifier}+Shift+3" = "move container to workspace number 3";
        "${modifier}+Shift+4" = "move container to workspace number 4";
        "${modifier}+Shift+5" = "move container to workspace number 5";
        "${modifier}+Shift+6" = "move container to workspace number 6";
        "${modifier}+Shift+7" = "move container to workspace number 7";
        "${modifier}+Shift+8" = "move container to workspace number 8";
        "${modifier}+Shift+9" = "move container to workspace number 9";

        "${modifier}+grave" = "workspace back_and_forth";

        "${modifier}+Shift+z" = "mark z; move scratchpad";
        "${modifier}+z" = "[con_mark=z] scratchpad show";
        "${modifier}+s" = "mode scratchpad";
        "${modifier}+r" = "mode resize";
        "${modifier}+y" = "mode workspace";

        "${modifier}+Alt+e" = "exec emacsclient -a '' -c";
        "${modifier}+Alt+b" = "exec firefox";
        "${modifier}+Alt+c" = "mode chat";

        "${modifier}+Shift+q" = "exec bash ~/.nixos-config/config/scripts/exit";
        "${modifier}+Control+q" =
          "exec bash ~/.nixos-config/config/scripts/kill";
        "${modifier}+Control+r" =
          "exec bash ~/.nixos-config/config/scripts/configurations";
        "${modifier}+F1" = "exec bash ~/.nixos-config/config/scripts/man";
        "${modifier}+F2" = "exec bash ~/.nixos-config/config/scripts/websearch";
        "${modifier}+F3" =
          "exec echo $(sxiv -t -o ~/Pictures/Wallpapers) | xargs -I {} cp {} ~/.cache/current.png && swaymsg output '*' bg ~/.cache/current.png fill";
        "${modifier}+F4" = "exec bash ~/.nixos-config/config/scripts/wifi";

        "${modifier}+Insert" =
          "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot --notify save screen ${screenshot_dir}";
        "${modifier}+Shift+Insert" =
          "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot copy area";

        "Control+Alt+space" = "exec makoctl dismiss -a";
        "Control+Alt+comma" = "exec makoctl restore";
        "Control+Alt+bracketleft" =
          "exec notify-send 'Notifications on'; exec makoctl set-mode default";
        "Control+Alt+bracketright" = "exec makoctl set-mode do-not-disturb";

        "XF86AudioLowerVolume" = "exec ${pkgs.pamixer}/bin/pamixer -d 10";
        "XF86AudioLowerVolume+Shift" =
          "exec ${pkgs.pamixer}/bin/pamixer -d 10 --allow-boost";
        "XF86AudioRaiseVolume" = "exec ${pkgs.pamixer}/bin/pamixer -i 10";
        "XF86AudioRaiseVolume+Shift" =
          "exec ${pkgs.pamixer}/bin/pamixer -i 10 --allow-boost";
        "XF86AudioMute" = "exec ${pkgs.pamixer}/bin/pamixer -t";

        "XF86MonBrightnessDown" = "exec ${pkgs.light}/bin/light -U 10";
        "XF86MonBrightnessUp" = "exec ${pkgs.light}/bin/light -A 10";

        "XF86AudioPlay" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
        "XF86AudioNext" = "exec ${pkgs.playerctl}/bin/playerctl next";
        "XF86AudioPrev" = "exec ${pkgs.playerctl}/bin/playerctl previous";
      };
    };
  };
  home-manager.users.alternateved.programs.i3status-rust = {
    enable = true;
    bars.top = {
      blocks = [
        {
          block = "maildir";
          inboxes = [
            "/home/alternateved/.mail/outside/Inbox"
            "/home/alternateved/.mail/inside/Inbox"
            "/home/alternateved/.mail/traffic/Inbox"
          ];
          threshold_warning = 1;
          threshold_critical = 10;
          display_type = "new";
          icon = true;
        }
        {
          block = "weather";
          format = "{weather_verbose} {temp}C";
          icons_format = " ";
          service = {
            name = "openweathermap";
            api_key = "f5c2f7ddce129955c85c723e691eab9b";
            city_id = "3093133";
            units = "metric";
          };
        }
        {
          block = "cpu";
          format = "{utilization}";
        }
        {
          block = "memory";
          format_mem = "{mem_used_percents}";
          display_type = "memory";
          icons_format = " {icon} ";
          icons = true;
          clickable = false;
          warning_mem = 80;
          critical_mem = 95;
        }
        {
          block = "sound";
          format = "{volume}";
          on_click = "pamixer -t";
        }
        {
          block = "battery";
          icons_format = " {icon} ";
          format = "{percentage} {time}";
          full_format = "{percentage} ";
          full_threshold = 96;
        }
        {
          block = "time";
          icons_format = " ";
          format = "%A, %b %_d";
        }
        {
          block = "time";
          icons_format = " ";
          format = "%H:%M";
        }
      ];

      settings = {
        icons_format = " {icon}  ";
        icons = {
          name = "material-nf";
          overrides = {
            cpu = "";
            time = "";
          };
        };
        theme = {
          name = "native";
          overrides = {
            idle_bg = theme.background;
            idle_fg = theme.foreground;
            separator = "|";
          };
        };
      };
    };
  };

  home-manager.users.alternateved.home.sessionVariables = {
    XDG_SESSION_TYPE = "wayland";
    XDG_SESSION_DESKTOP = "sway";
    XDG_CURRENT_DESKTOP = "sway";
    SDL_VIDEODRIVER = "wayland";
    GDK_BACKEND = "wayland";
    QT_QPA_PLATFORM = "wayland";
    BMENU_BACKEND = "wayland";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = 1;
    _JAVA_AWT_WM_NONPARENTING = 1;
    MOZ_ENABLE_WAYLAND = 1;
    MOZ_DBUS_REMOTE = 1;
    MOZ_USE_XINPUT2 = 1;
  };
}
