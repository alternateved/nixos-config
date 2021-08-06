{ config, lib, pkgs, ... }:

{

  programs = {
    emacs = {
      enable = true;
      package = pkgs.emacsGcc;
    };

  };

  services = {
    emacs.package = pkgs.emacsUnstable;

  };
}
