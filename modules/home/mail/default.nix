{ pkgs, ... }: {
  services.mbsync = {
    enable = true;
    frequency = "10m";
    configFile = ../../../config/mbsync/mbsyncrc;
  };
  home.packages = with pkgs; [ mu isync ];
}
