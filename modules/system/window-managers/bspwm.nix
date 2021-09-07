{ pkgs, ... }: {
  imports = [ ../polybar ../rofi ];
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
        xrdb ~/.Xresources
      '';
    };
    windowManager.bspwm.enable = true;
  };
  environment.systemPackages = with pkgs; [ xorg.xwininfo ];
}
