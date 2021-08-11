{ pkgs, ... }: {
  services.polybar = {
    enable = true;
    package = pkgs.polybar.override { alsaSupport = true; };
    config = ../config/polybar/config.ini;
    script = ''
      for m in $(polybar --list-monitors | cut -d":" -f1); do
          MONITOR=$m polybar -q main &
          MONITOR=$m polybar -q aux &
      done
    '';
  };
}
