{ pkgs, ... }: {
  services.polybar = {
    enable = true;
    package = pkgs.polybar.override {
      alsaSupport = true;
      githubSupport = true;
    };
    config = ../../config/polybar/config.ini;
    extraConfig = ''
      [module/spotify]
      type = custom/script
      exec = ${pkgs.playerctl}/bin/playerctl --player=spotify metadata --format "{{ artist }} - {{ title }}"
      format-prefix = "  "
      click-left = ${pkgs.playerctl}/bin/playerctl --player=spotify play-pause
    '';
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
