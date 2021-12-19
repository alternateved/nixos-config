_: pkgs: rec {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      (self: super: rec {
        alternateved-xmonad = self.callCabal2nix "alternateved-xmonad"
          (pkgs.lib.sourceByRegex ../config/xmonad [
            "xmonad.hs"
            "alternateved-xmonad.cabal"
          ])
          { };
        alternateved-xmobar = self.callCabal2nix "alternateved-xmobar"
          (pkgs.lib.sourceByRegex ../config/xmobar [
            "xmobar.hs"
            "alternateved-xmobar.cabal"
          ])
          { };
      });
  });
  sway = pkgs.sway.overrideAttrs (old: rec {
    src = pkgs.fetchFromGitHub {
      owner = "swaywm";
      repo = "sway";
      rev = "bb7bb3676deead149c66fbf74b55d3bb4f9d69b5";
      sha256 = "/mkeoUq9n6ybTz2gR7Kgb6mCdDZVN39gF0V0K1htGas=";
    };
  });
}
