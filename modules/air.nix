{ pkgs, config, lib, ... }: {
  programs.mbsync = {
    extraConfig = builtins.readFile ../../config/mbsync/mbsyncrc;
    enable = true;
  };

  home.packages = with pkgs; [ mu ];
}
