{ pkgs, ... }: {
  programs.mbsync = {
    extraConfig = builtins.readFile ../../../config/mbsync/mbsyncrc;
    enable = true;
  };
  services.mbsync = {
    enable = true;
    frequency = "10m";
  };
  home.packages = with pkgs; [ mu ];
}
