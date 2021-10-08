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
      rev = "83310f5abf1fd9937a9f734da4015aacd54fd112";
      sha256 = "hEmY5OwMtlMif0nY/17SfIg1lzC/TCEA7pMhbwwBOwo=";
    };
  });
  bemenu = pkgs.bemenu.overrideAttrs (old: rec {
    src = pkgs.fetchFromGitHub {
      owner = "swaywm";
      repo = "sway";
      rev = "b7f8db7128b2560460b06c7b29adadd4e7874c93";
      sha256 = "TFlLxY3LhiWkorAoottEW8b17PULqUIWcWrUiCgMpBE=";
    };
  });
}
