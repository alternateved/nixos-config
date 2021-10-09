_: pkgs: rec {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      (self: super: rec {
        alternateved-xmonad = self.callCabal2nix "alternateved-xmonad"
          (pkgs.lib.sourceByRegex ../config/xmonad [
            "xmonad.hs"
            "alternateved-xmonad.cabal"
          ]) { };
        alternateved-xmobar = self.callCabal2nix "alternateved-xmobar"
          (pkgs.lib.sourceByRegex ../config/xmobar [
            "xmobar.hs"
            "alternateved-xmobar.cabal"
          ]) { };
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
      rev = "624ffa45162d7df1ae0796b283bef2f4a0d6a0d7";
      sha256 = "CRlCeQBrw9AK1RqKwvxmy/Ge039UvMLD32SJcHIlLig=";
    };
  });
  bemenu = pkgs.bemenu.overrideAttrs (old: rec {
    src = pkgs.fetchFromGitHub {
      owner = "Cloudef";
      repo = "bemenu";
      rev = "b7f8db7128b2560460b06c7b29adadd4e7874c93";
      sha256 = "TFlLxY3LhiWkorAoottEW8b17PULqUIWcWrUiCgMpBE=";
    };
  });
}
