{ pkgs, ... }: {

  imports = [
    ../alacritty
    ../direnv
    ../dunst
    ../emacs
    ../firefox
    ../flameshot
    ../fzf
    ../git
    ../htop
    ../lightdm
    ../mail
    ../mpv
    ../ncspot
    ../nnn
    ../neovim
    ../picom
    ../polybar
    ../redshift
    ../starship/lambda.nix
    ../theme
    ../xdg
    ../zathura
    ../zsh
  ];

  services.xserver = {
    enable = true;
    layout = "pl";
    xkbOptions = "caps:escape_shifted_capslock";
    libinput = {
      enable = true;
      touchpad.disableWhileTyping = true;
      touchpad.middleEmulation = true;
      touchpad.naturalScrolling = true;
    };
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
