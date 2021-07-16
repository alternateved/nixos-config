{ pkgs ? import <nixpkgs> { } }:

with pkgs;
let
  haskellTooling =
    let ghc' = haskellPackages.ghcWithPackages (hp: with hp; [ xmobar ]);
    in [ ghc' ghcid ];

in mkShell { buildInputs = haskellTooling; }
