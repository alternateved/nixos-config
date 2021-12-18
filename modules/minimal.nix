{ config, lib, pkgs,  ... }: {
  programs.home-manager.enable = true;
  home.username = "alternateved";
  home.homeDirectory = "/home/alternateved";
  home.packages = with pkgs; [ unzip ];
  home.stateVersion = "21.11";
}
