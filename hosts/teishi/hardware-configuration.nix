{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "i915" "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  # boot.blacklistedKernelModules = [ "nouveau" "nvidia" ];

  fileSystems."/" =
    { device = "/dev/mapper/nixenc";
      fsType = "btrfs";
      options = [ "subvol=nixos" ];
    };

  boot.initrd.luks.devices."nixenc".device = "/dev/disk/by-uuid/0b1950c3-8fd5-4402-a757-5fc7bb857070";

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/383A-EC18";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/3379e138-81de-401c-8ec2-ed925e0063fe"; }
    ];

  powerManagement.cpuFreqGovernor = "performance";
  hardware.cpu.intel.updateMicrocode = true;
  # hardware.nvidiaOptimus.disable = true;
  hardware.acpilight.enable = true;
  hardware.opengl.extraPackages = with pkgs; [
    vaapiIntel
    vaapiVdpau
    libvdpau-va-gl
    intel-media-driver
  ];
}
