{ pkgs }:

with pkgs;
let
  cLibsAndTools =
    [ pkg-config xorg.libX11 xorg.libXinerama xorg.libXrandr xorg.libXext ];

  haskellTools = [
    cabal-install
    ghcid
    haskellPackages.hindent
    haskellPackages.hoogle
    haskellPackages.ormolu
  ];

in mkShell {
  name = "flakeEnv";
  buildInputs = cLibsAndTools ++ haskellTools ++ [ rnix-lsp ];
}
