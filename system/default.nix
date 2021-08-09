{ config, pkgs, ... }:

let colors = import ../home/modules/colors.nix;
in {
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  users = {
    defaultUserShell = pkgs.fish;
    users.alternateved = {
      isNormalUser = true;
      extraGroups = [ "wheel" "networkmanager" "audio" "video" ];
    };
  };

  nixpkgs.config.allowUnfree = true;

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";
      };
      grub = {
        enable = true;
        version = 2;
        device = "nodev";
        efiSupport = true;
        enableCryptodisk = true;
      };
    };
    kernel.sysctl = { "vm.swappiness" = 10; };
    kernelPackages = pkgs.linuxPackages_latest;
  };

  networking = {
    hostName = "nixos";
    firewall.enable = true;
    networkmanager.enable = true;
    useDHCP = false;
    interfaces.enp3s0.useDHCP = true;
    interfaces.wlp2s0.useDHCP = true;
  };

  time.timeZone = "Europe/Warsaw";

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "pl";
  };

  sound.enable = true;
  hardware = {
    pulseaudio = { enable = true; };

    bluetooth.enable = true;
  };

  services = {
    xserver = {
      enable = true;
      layout = "pl";
      xkbOptions = "caps:escape_shifted_capslock";

      displayManager = {
        defaultSession = "none+xmonad";
        autoLogin.enable = true;
        autoLogin.user = "alternateved";
        sessionCommands = ''
          xsetroot -cursor_name left_ptr &
          autorandr -c &
          bluetoothctl power on
          nitrogen --restore &
        '';
        lightdm.greeters.mini = {
          enable = true;
          user = "alternateved";
          extraConfig = ''
            [greeter-theme]
            background-image = "";
            background-color = ${colors.background}
            text-color = ${colors.foreground}
            password-background-color = ${colors.gray}
            window-color = ${colors.bright-black}
            border-color = ${colors.foreground}
          '';
        };

      };

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
      # windowManager.session = [{
      #   name = "xmonad";
      #   start = ''
      #     /usr/bin/env alternateved-xmonad &
      #     waitPID=$!
      #   '';
      # }];

      libinput = {
        enable = true;
        touchpad.disableWhileTyping = true;
        touchpad.middleEmulation = true;
      };
    };
    tlp.enable = true;
    gvfs.enable = true;
  };

  environment.systemPackages = with pkgs; [
    coreutils
    vim
    wget
    git
    mesa
    killall
    xorg.xkill
    # haskellPackages.xmonad
    # haskellPackages.alternateved-xmonad
  ];

  fonts.fonts = with pkgs; [
    font-awesome_5
    (iosevka-bin.override { variant = "aile"; })
    (iosevka-bin.override { variant = "etoile"; })
    (nerdfonts.override { fonts = [ "Iosevka" ]; })
  ];

  nix = {
    autoOptimiseStore = true;
    gc = {
      automatic = true;
      dates = "monthly";
      options = "--delete-older-than 8d";
    };
    optimise = {
      automatic = true;
      dates = [ "weekly" ];
    };
    trustedUsers = [ "alternateved" "root" ];
  };

  nix.package = pkgs.nixUnstable;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  system.stateVersion = "21.05";
}