{ config, lib, pkgs, ... }: {
  programs = {
    emacs = {
      enable = true;
      package = pkgs.emacsGcc;
      extraPackages = (epkgs: [ epkgs.vterm ]);
    };
  };

  services = { emacs.enable = true; };

  home = {
    sessionVariables = { DOOMDIR = "home/alternateved/.config/doom"; };

    file.".doom.d" = {
      source = ../config/doom;
      recursive = true;
      onChange = "doom -y sync -u";
    };
  };
}
