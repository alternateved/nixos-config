{ pkgs, ... }: {
  home-manager.users.alternateved = {
    systemd.user.services = {
      libinput-gestures = {
        Unit = {
          Description = "Set libinput-gestures service";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };

        Install = { WantedBy = [ "graphical-session.target" ]; };

        Service = {
          Type = "simple";
          RemainAfterExit = "yes";
          ExecStart = "${pkgs.libinput-gestures}/bin/libinput-gestures";
        };
      };
    };

   home.file.".config/libinput-gestures.conf".source = pkgs.writeText "libinput-gestures.conf" ''
      # Cycle right through sway workspaces
      gesture: swipe right 3 swaymsg workspace prev

      # Cycle left through sway workspaces
      gesture: swipe left 3 swaymsg workspace next

      # Toggle fullscreen
      gesture: swipe up 4 swaymsg fullscreen toggle
      gesture: swipe down 4 swaymsg fullscreen toggle

      # Toggle bar
      gesture: swipe up 3 swaymsg bar mode toggle
      gesture: swipe down 3 swaymsg bar mode toggle
     '';

   home.packages = with pkgs; [ libinput-gestures ];
  };
}
