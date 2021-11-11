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
  bspwm = pkgs.bspwm.overrideAttrs (old: rec {
    src = pkgs.fetchFromGitHub {
      owner = "baskerville";
      repo = "bspwm";
      rev = "e22d0fad23e0e85b401be69f2360a1c3a0767921";
      sha256 = "ds+jiDp0hwycfz/YQQy74SOcVmxOqxI9EYoJjp1Vs+E=";
    };
  });
  sway = pkgs.sway.overrideAttrs (old: rec {
    src = pkgs.fetchFromGitHub {
      owner = "swaywm";
      repo = "sway";
      rev = "fc25e4944efdc5bc7e33a81180908927dba93ee6";
      sha256 = "0gZP2Pe2LsMzScKKRL/q98ERJQuqxa1Swwi9DY/KCvg=";
    };
  });
  bemenu = pkgs.bemenu.overrideAttrs (old: rec {
    src = pkgs.fetchFromGitHub {
      owner = "Cloudef";
      repo = "bemenu";
      rev = "d593ab27b6444ccc5765edbdefcb182c64181ebf";
      sha256 = "29NfRmtsSew8w4+uliEJ8FjVthSrhTTOorzwziCrbpE=";
    };
  });
}
