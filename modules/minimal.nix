{ config, lib, pkgs, inputs, ... }:

{
  programs.home-manager.enable = true;
  home.username = "alternateved";
  home.homeDirectory = "/home/alternateved";

  imports = [ ./direnv ./fzf ./git ./htop ./neovim ./starship/pi.nix ./zsh ];

  home.packages = with pkgs; [ gcc xarchiver unzip exa xclip killall nnn ];

  home.stateVersion = "21.05";
}
