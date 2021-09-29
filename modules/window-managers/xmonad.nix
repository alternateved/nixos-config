{ pkgs, ... }: {

  imports = [
    ../alacritty
    ../dunst
    ../emacs
    ../firefox
    ../flameshot
    ../fzf
    ../git
    ../htop
    ../mail
    ../mpv
    ../ncspot
    ../nnn
    ../neovim
    ../picom
    ../redshift
    ../starship/lambda.nix
    ../theme
    ../xdg
    ../zathura
    ../zsh
  ];

  services.xserver = {
    displayManager = {
      defaultSession = "none+myxmonad";
      sessionCommands = ''
        bluetoothctl power on
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
    haskellPackages.xmonad
    haskellPackages.alternateved-xmobar
    haskellPackages.alternateved-xmonad
  ];
}
