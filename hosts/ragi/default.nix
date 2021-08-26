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
      firewall.enable = true;
      networkmanager.enable = true;
    };

    environment.systemPackages = with pkgs; [ coreutils wget git ];

    services = {
      openssh.enable = true;
      nextcloud = {
        enable = true;
        hostName = "testing";
      };
    };

    users = {
      mutableUsers = false;
      defaultUserShell = pkgs.zsh;
      users.alternateved = {
        isNormalUser = true;
        openssh.authorizedKeys.keys = [
          "ssh-dss AAAAB3NzaC1kc3MAAACBAKQuH68mhwVo6jC92clfc/nUpqpi98Q0MZxpbCWOhk/kdXFMgfuq8WBJtdPnXAz8OiShll9BCyabblkN6o13gbn24NUdeXYr7yDDh8fzDVxrRsv+LM5KcANMkfnAnWg5iyQcglfL4R5qlIJoahOuiNIHrCr6Q61vqWPDGSGTXO2dAAAAFQCqfrTEX0wnoYpwm8zCi5IepS1IxwAAAIEAlWWflYgROzjgx0OJ+BbMX6qz4MLv3trFCs2JQ+uAwMaNmw+Cgp4zZKEmQ4pLS6C0SwhLILAnWumRWG82ghWyvbNCwirislqkNykcb7CaSoMOreEmoZWLvH25VYTa6/Au7carIYmNPFHk7HnnqpX0iyAWtg8NY+v6Yn0Np0Z+7KoAAACBAJQf6guV6q73O3bZwFw1sM9179WV+XAb6x3tiI80L05iEHiFyqnImuLoCrRoGwOQ4gE0ckMYf5r9gIn3q0a8NPm5aRGV6prpfCw2eMYStjtxPoAA7gNRx0ZkE8ppRoefcT0w4GhAJgmGP8JQCgoAu5QBStCgq53pnT6wZc3TYyzB"
        ];
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
