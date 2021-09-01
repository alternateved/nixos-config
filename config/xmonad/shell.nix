{ pkgs ? import <nixpkgs> { } }:

with pkgs;
let
  haskellTooling = let
    ghc' = haskellPackages.ghcWithPackages
      (hp: with hp; [ xmobar xmonad xmonad-contrib ]);
  in [ ghc' ];

in mkShell { buildInputs = haskellTooling; }
