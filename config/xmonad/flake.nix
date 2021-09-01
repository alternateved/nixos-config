{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    xmonad = {
      url = "github:xmonad/xmonad";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    xmonad-contrib = {
      url = "github:xmonad/xmonad-contrib";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, flake-utils, nixpkgs, xmonad, xmonad-contrib }:
    let overlays = [ xmonad.overlay xmonad-contrib.overlay ];
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          config.allowBroken = true;
        };
      in rec {
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ p.alternateved-xmonad p.xmonad-contrib ];
          buildInputs = with pkgs.haskellPackages; [
            cabal-install
            haskell-language-server
            hlint
            ghcid
            ormolu
            implicit-hie
          ];
        };
        defaultPackage = pkgs.haskellPackages.alternateved-xmonad;
      }) // {
        inherit overlays;
      };
}
