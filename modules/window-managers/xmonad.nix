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
      xkbVariant = "colemak";
      xkbOptions = "ctrl:nocaps";
      libinput = {
        enable = true;
        touchpad.disableWhileTyping = true;
        touchpad.middleEmulation = true;
        touchpad.naturalScrolling = true;
        mouse.disableWhileTyping = true;
        mouse.naturalScrolling = true;
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
    xfce.thunar
    xfce.thunar-volman
    xfce.thunar-archive-plugin
    xdotool
    xwallpaper
    xsecurelock
    xorg.xkill
    haskellPackages.xmonad
    haskellPackages.alternateved-xmobar
    haskellPackages.alternateved-xmonad
  ];
}
