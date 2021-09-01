{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    xmobar = {
      url = "github:jaor/xmobar";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, flake-utils, nixpkgs, xmobar }:
    let overlays = [ xmobar.overlay ];
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          config.allowBroken = true;
        };
      in rec {
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ p.alternateved-xmobar ];
          buildInputs = with pkgs.haskellPackages; [
            cabal-install
            haskell-language-server
            hlint
            ghcid
            ormolu
            implicit-hie
          ];
        };
        defaultPackage = pkgs.haskellPackages.alternateved-xmobar;
      }) // {
        inherit overlays;
      };
}
