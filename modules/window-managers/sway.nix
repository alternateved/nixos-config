{ pkgs, ... }:
let theme = import ../theme/colors.nix;
in {
  imports = [
    ../alacritty
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

  services.xserver.displayManager.defaultSession = "sway";
  programs.sway = {
    enable = true;
    extraSessionCommands = ''
      export XDG_SESSION_TYPE=wayland
      export SDL_VIDEODRIVER=wayland
      export GDK_BACKEND=wayland
      export QT_QPA_PLATFORM=wayland
      export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
      export _JAVA_AWT_WM_NONPARENTING=1
      export MOZ_ENABLE_WAYLAND=1
      export MOZ_USE_XINPUT2=1
      export XDG_CURRENT_DESKTOP=sway
      export BMENU_BACKEND=wayland
    '';
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

      focus = {
        followMouse = "always";
        mouseWarping = true;
      };

      gaps = {
        inner = 5;
        outer = 5;
        smartGaps = true;
        smartBorders = "on";
      };

      floating.border = 2;
      window.border = 2;
      window.titlebar = false;
      window.commands = [
        {
          criteria = { app_id = "mpv"; };
          command = "floating enable";
        }
        {
          criteria = { title = "^(.*) Indicator"; };
          command = "floating enable";
        }
        {
          criteria = { class = "Sxiv"; };
          command = "floating enable";
        }
        {
          criteria = { app_id = "terrm"; };
          command =
            "floating enable, resize set width 900px 600px, move scratchpad, scratchpad show";
        }
        {
          criteria = { app_id = "mixerr"; };
          command =
            "floating enable, resize set width 1000px 500px, move scratchpad, scratchpad show";
        }
        {
          criteria = { app_id = "monitorr"; };
          command =
            "floating enable, resize set width 1200px 900px, move scratchpad, scratchpad show";
        }
        {
          criteria = { app_id = "spotiff"; };
          command =
            "floating enable, resize set width 1300px 500px, move scratchpad, scratchpad show";
        }
        {
          criteria = { title = "scratcher"; };
          command = "floating enable, move scratchpad, scratchpad show";
        }
      ];
      startup = [{
        always = true;
        command = ''
          swayidle -w timeout 600 'swaymsg "output * dpms off"' resume 'swaymsg "output * dpms on"' '';
      }];

      assigns = {
        "1" = [{ class = "Firefox"; }];
        "2" = [ { class = "discord"; } { class = "Signal"; } ];
        "5" = [ { app_id = "mpv"; } { class = "Spotify"; } ];
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

        };
        mode = "dock";
        position = "top";
        trayOutput = "none";
        statusCommand =
          "${pkgs.i3status-rust}/bin/i3status-rs ~/.config/i3status-rust/config-top.toml";
      }];

      modes = {

        scratchpad = {
          "t" = "exec ${terminal} --class terrm, mode default";
          "v" = "exec ${terminal} --class mixerr -e pulsemixer, mode default";
          "m" = "exec ${terminal} --class monitorr -e htop, mode default";
          "s" = "exec ${terminal} --class spotiff -e ncspot, mode default";
          "e" =
            "exec emacsclient -a '' --eval \"(open-scratch-frame)\", mode default";
          Return = ''mode "default"'';
          Escape = ''mode "default"'';
        };

        "Exit Sway: [l]ock, [e]xit, [r]eboot, [p]oweroff, [s]uspend" = {
          "p" = "exec ${pkgs.systemd}/bin/systemctl poweroff, mode default";
          "r" = "exec ${pkgs.systemd}/bin/systemctl reboot, mode default";
          "s" = "exec ${pkgs.systemd}/bin/systemctl suspend, mode default";
          "l" = "exec ${pkgs.swaylock}/bin/swaylock -f -c 000000, mode default";
          "e" = "exec swaymsg exit, mode default";
          "Escape" = "mode default";
          "Return" = "mode default";
        };
      };

      terminal = "${pkgs.alacritty}/bin/alacritty";
      menu = "exec bash " + ../../config/scripts/menu;

      keybindings = {
        "${modifier}+Shift+Return" = "exec ${terminal}";
        "${modifier}+Shift+c" = "kill";
        "${modifier}+Shift+r" = "reload";
        "${modifier}+Shift+q" =
          "mode 'Exit Sway: [l]ock, [e]xit, [r]eboot, [p]oweroff, [s]uspend'";

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

        "${modifier}+v" = "split toggle";
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
        "${modifier}+0" = "workspace number 10";

        "${modifier}+Shift+1" = "move container to workspace number 1";
        "${modifier}+Shift+2" = "move container to workspace number 2";
        "${modifier}+Shift+3" = "move container to workspace number 3";
        "${modifier}+Shift+4" = "move container to workspace number 4";
        "${modifier}+Shift+5" = "move container to workspace number 5";
        "${modifier}+Shift+6" = "move container to workspace number 6";
        "${modifier}+Shift+7" = "move container to workspace number 7";
        "${modifier}+Shift+8" = "move container to workspace number 8";
        "${modifier}+Shift+9" = "move container to workspace number 9";
        "${modifier}+Shift+0" = "move container to workspace number 10";

        "${modifier}+grave" = "workspace back_and_forth";

        "${modifier}+Shift+z" = "move scratchpad";
        "${modifier}+z" = "scratchpad show";
        "${modifier}+s" = "mode scratchpad";

        "${modifier}+Alt+e" = "exec emacsclient -a '' -c";
        "${modifier}+Alt+b" = "exec firefox";

        "${modifier}+Control+q" =
          "exec bash ~/.nixos-config/config/scripts/kill";
        "${modifier}+Control+r" =
          "exec bash ~/.nixos-config/config/scripts/configurations";
        "${modifier}+F1" = "exec bash ~/.nixos-config/config/scripts/man";
        "${modifier}+F2" = "exec bash ~/.nixos-config/config/scripts/websearch";
        "${modifier}+F3" =
          "exec echo $(sxiv -t -o ~/Pictures/Wallpapers) > /home/alternateved/.cache/wall";
        "${modifier}+F4" = "exec bash ~/.nixos-config/config/scripts/youtube";
        "${modifier}+F5" = "exec bash ~/.nixos-config/config/scripts/wifi";

        "${modifier}+Insert" =
          "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot --notify save screen";
        "${modifier}+Shift+Insert" =
          "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot copy area";

        "Control+Alt+Space" = "exec makoctl -a";

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
          format_mem = "{mem_used}/{mem_total}";
          display_type = "memory";
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
          format = "{percentage} {time}";
          full_format = "{percentage}";
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
        icons_format = "  {icon}  ";
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
}

