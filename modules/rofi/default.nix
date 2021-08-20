{ pkgs, ... }: {
  programs.rofi = {
    enable = true;
    theme = ../../config/rofi/config.rasi;
  };
}
