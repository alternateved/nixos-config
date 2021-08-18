{ config, lib, pkgs, ... }: {
  programs = {
    emacs = {
      enable = true;
      package = pkgs.emacsGcc;
      extraPackages = (epkgs: [ epkgs.vterm ]);
    };
  };
  services = { emacs.enable = true; };
}
