{ config, lib, pkgs, inputs, ... }:

{
  programs.home-manager.enable = true;
  home.username = "alternateved";
  home.homeDirectory = "/home/alternateved";

  imports = [
    ./direnv
    ./fzf
    ./git
    ./htop
    ./neovim
    ./starship/pi.nix
    ./theme/xresources.nix
    ./zsh
  ];

  home.packages = with pkgs; [ xarchiver unzip exa xclip killall nnn neofetch ];

  home.stateVersion = "21.05";
}
