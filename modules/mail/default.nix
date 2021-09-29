{ pkgs, ... }: {
  home-manager.users.alternateved.programs.mbsync = {
    extraConfig = builtins.readFile ../../config/mbsync/mbsyncrc;
    enable = true;
  };
  home-manager.users.alternateved.services.mbsync = {
    enable = true;
    frequency = "10m";
  };
  home-manager.users.alternateved.home.packages = with pkgs; [ mu ];
}
