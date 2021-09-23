{ pkgs, ... }: {
  imports = [ ../polybar ];
  nixpkgs.overlays = [
    (self: super: {
      bspwm = super.bspwm.overrideAttrs (old: rec {
        src = pkgs.fetchFromGitHub {
          owner = "baskerville";
          repo = "bspwm";
          rev = "e22d0fad23e0e85b401be69f2360a1c3a0767921";
          sha256 = "ds+jiDp0hwycfz/YQQy74SOcVmxOqxI9EYoJjp1Vs+E=";
        };
      });
    })
  ];
  services.xserver = {
    displayManager = {
      defaultSession = "none+bspwm";
      sessionCommands = ''
        bluetoothctl power on
      '';
    };
    windowManager.bspwm.enable = true;
  };
  environment.systemPackages = with pkgs; [
    (dmenu.overrideAttrs (oldAttrs: rec {
      patches = [
        (fetchpatch {
          url =
            "https://tools.suckless.org/dmenu/patches/border/dmenu-border-20201112-1a13d04.diff";
          sha256 = "1ghckggwgasw9p87x900gk9v3682d6is74q2rd0vcpsmrvpiv606";
        })
        (fetchpatch {
          url =
            "https://tools.suckless.org/dmenu/patches/center/dmenu-center-20200111-8cd37e1.diff";
          sha256 = "0x7jc1m0138p7vfa955jmfhhyc317y0wbl8cxasr6cfpq8nq1qsg";
        })
      ];
    }))
    killall
    xdo
    xtitle
    xwallpaper
    xsecurelock
    xorg.xkill
    xorg.xwininfo
  ];
}
