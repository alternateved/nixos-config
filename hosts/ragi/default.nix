
  { config, pkgs, lib, ... }:

  {
    imports = ["${fetchTarball "https://github.com/NixOS/nixos-hardware/archive/936e4649098d6a5e0762058cb7687be1b2d90550.tar.gz" }/raspberry-pi/4"];

    fileSystems = {
      "/" = {
        device = "/dev/disk/by-label/NIXOS_SD";
        fsType = "ext4";
        options = [ "noatime" ];
      };
    };

    boot = {
      kernelPackages = pkgs.linuxPackages_rpi4;
    };

    networking = {
      hostName = "nixos-pi";
      networkmanager.enable = true;
    };

    environment.systemPackages = with pkgs; [ neovim git ];

    services.openssh.enable = true;

    users = {
      mutableUsers = false;
      defaultUserShell = pkgs.zsh;
      users.alternateved = {
        isNormalUser = true;
	openssh.authorizedKeys.keyFiles = [ /home/alternateved/.ssh/authorized_keys ];
	passwordFile = "/home/alternateved/.secrets/pass";
        extraGroups = [ "wheel" "networkmanager" "audio" ];
      };
    };

    hardware = {
	raspberry-pi."4".fkms-3d.enable = true;
    	pulseaudio.enable = true;
    };

    services.xserver = {
      enable = true;
      layout = "pl";
      xkbOptions = "caps:escape_shifted_capslock";
      displayManager = {
	defaultSession = "none+herbstluftwm";
	autoLogin.enable = true;
	autoLogin.user = "alternateved";
	lightdm.greeters.mini = {
	  enable = true;
	  user = "alternateved";
	};
      };
      windowManager.herbstluftwm.enable = true;
    };
    nix = {
	trustedUsers = [ "alternateved" "root" ];
	package = pkgs.nixUnstable;
	extraOptions = ''
	  experimental-features = nix-command flakes
	'';
    };
  }
