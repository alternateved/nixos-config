{ config, pkgs, ... }:

{
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
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
    };

    bluetooth.enable = true;
  };

  services = {
    xserver = {
      enable = true;
      layout = "pl";
      xkbOptions = "caps:escape_shifted_capslock";

      displayManager = {
        # defaultSession = "none+xmonad";
        defaultSession = "none+herbstluftwm";
        autoLogin.enable = true;
        autoLogin.user = "alternateved";
        lightdm.greeters.mini = {
          enable = true;
          user = "alternateved";
          extraConfig = ''
            [greeter-theme]
            background-image = "";
            background-color = "#1d1f21"
            text-color = "#c4c8c5"
            password-background-color = "#545B68"
            window-color = "#181a23"
            border-color = "#c4c8c5"
          '';
        };
      };

      windowManager.herbstluftwm.enable = true;

      # windowManager.xmonad = {
      #   enable = true;
      #   enableContribAndExtras = true;
      #   extraPackages = haskellPackages: [
      #     haskellPackages.xmonad
      #     haskellPackages.xmonad-contrib
      #     haskellPackages.xmobar
      #   ];
      # };

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
    vim
    wget
    curl
    git
    mesa
    autorandr
    htop
    xorg.xkill
    killall
    xdotool
  ];

  fonts.fonts = with pkgs; [
    dejavu_fonts
    fira
    font-awesome_5
    (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
  ];

  nix = {
    gc = {
      automatic = true;
      dates = "monthly";
      options = "--delete-older-than 90d";
    };
    autoOptimiseStore = true;

    trustedUsers = [ "alternateved" "root" ];

    binaryCaches = [
      "https://nix-community.cachix.org/"
      "https://hydra.iohk.io" # Haskell.nix
      "https://ebn.cachix.org"
    ];

    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" # Haskell.nix
      "ebn.cachix.org-1:Z8QlCh2aXXURAyMpMgDh7VaHo/iTJ7P8r52fxpjJ5JA="
    ];
  };

  system.stateVersion = "21.05";

}
