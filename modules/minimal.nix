{ config, lib, pkgs, inputs, ... }:

{
  programs.home-manager.enable = true;
  home.username = "alternateved";
  home.homeDirectory = "/home/alternateved";

  imports = [ ./direnv ./fzf ./git ./htop ./neovim ./nnn ./starship/pi.nix ./zsh ];

  home.packages = with pkgs; [ xarchiver unzip ];

  home.stateVersion = "21.05";
}
