{ pkgs, ... }: {
  home-manager.users.alternateved.services.polybar = {
    enable = true;
    package = pkgs.polybar.override {
      pulseSupport = true;
      githubSupport = true;
    };
    config = ../../../config/polybar/config.ini;
    script = ''
      outputs=$(polybar --list-monitors | cut -d":" -f1)
      outputs_length=$(wc -w <<< "$outputs")

      if [ "$outputs_length" == 1 ]; then
          MONITOR=$m polybar -q main &
      else
          for m in $outputs; do
              MONITOR=$m polybar -q main &
              MONITOR=$m polybar -q aux &
          done
      fi
    '';
  };
}
