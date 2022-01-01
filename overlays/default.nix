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
      rev = "59aebaa5f9f3afe9cdfbb0d37c4dc631690da3b9";
      sha256 = "oAoSqS9DUtGjySvDGgJ74osEIq04MSVu0FBOOuBssY4=";
    };
  });
}
