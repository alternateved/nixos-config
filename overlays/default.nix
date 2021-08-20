_: pkgs: rec {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      (self: super: rec {
        alternateved-xmonad = self.callCabal2nix "alternateved-xmonad"
          (pkgs.lib.sourceByRegex ../config/xmonad [
            "xmonad.hs"
            "alternateved-xmonad.cabal"
          ]) { };
      });
  });
}
