{ pkgs, ... }: {
  services.xserver = {
    displayManager = {
      defaultSession = "none+myxmonad";
      sessionCommands = ''
        bluetoothctl power on
        xrdb ~/.Xresources
      '';
    };
    windowManager = {
      session = [{
        name = "myxmonad";
        start = ''
          /usr/bin/env alternateved-xmonad &
          waitPID=$!
        '';
      }];
    };
  };
  environment.systemPackages = with pkgs; [
    killall
    xdotool
    xwallpaper
    xsecurelock
    xorg.xkill
    xorg.xwininfo
    haskellPackages.xmonad
    haskellPackages.alternateved-xmobar
    haskellPackages.alternateved-xmonad
  ];
}
