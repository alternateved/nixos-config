{ pkgs, ... }: {

  imports = [
    ../kitty
    ../direnv
    ../dunst
    ../emacs
    ../firefox
    ../flameshot
    ../fzf
    ../git
    ../htop
    ../interception-tools
    ../lightdm
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

  services = {
    xserver = {
      enable = true;
      layout = "pl";
      xkbOptions = "ctrl:nocaps";
      libinput = {
        enable = true;
        touchpad.disableWhileTyping = true;
        touchpad.middleEmulation = true;
        touchpad.naturalScrolling = true;
      };
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
    tlp.enable = true;
  };

  environment.systemPackages = with pkgs; [
    pcmanfm
    xdotool
    xwallpaper
    xsecurelock
    xorg.xkill
    haskellPackages.xmonad
    haskellPackages.alternateved-xmobar
    haskellPackages.alternateved-xmonad
  ];
}
