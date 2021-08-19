  { config, pkgs, lib, ... }:

  {
    fileSystems = {
      "/" = {
        device = "/dev/disk/by-label/NIXOS_SD";
        fsType = "ext4";
        options = [ "noatime" ];
      };
    };

    boot = {
      loader = {
        grub.enable = false;
        raspberryPi = {
          enable = true;
          version = 4;
        };
      };
      kernelPackages = pkgs.linuxPackages_rpi4;
      initrd.availableKernelModules = [ "usbhid" "usb_storage" "vc4" ];
    };

    time.timeZone = "Europe/Warsaw";

    networking = {
      hostName = "ragi";
      networkmanager.enable = true;
    };

    environment.systemPackages = with pkgs; [ coreutils wget git ];

    services.openssh.enable = true;

    users = {
      mutableUsers = false;
      defaultUserShell = pkgs.zsh;
      users.alternateved = {
        isNormalUser = true;
        openssh.authorizedKeys.keyFiles =
          [ /home/alternateved/.ssh/authorized_keys ];
        passwordFile = "/home/alternateved/.secrets/pass";
        extraGroups = [ "wheel" "networkmanager" "audio" ];
      };
    };

    hardware = {
      raspberry-pi."4".fkms-3d.enable = true;
      enableRedistributableFirmware = true;
      pulseaudio.enable = true;
    };

    powerManagement.cpuFreqGovernor = "ondemand";

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
      binaryCaches = [ "https://nix-community.cachix.org/" ];
      binaryCachePublicKeys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
      package = pkgs.nixUnstable;
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
    };
  }
