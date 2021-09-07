{ pkgs, ... }:

let nnn = pkgs.nnn.override { withNerdIcons = true; };
in { home.packages = with pkgs; [ nnn ]; }
