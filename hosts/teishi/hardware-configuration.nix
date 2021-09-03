{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules =
    [ "i915" "xhci_pci" "ahci" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  boot.blacklistedKernelModules = [ "nouveau" "nvidia" ];

  boot.initrd = {
    luks.devices."root" = {
      device =
        "/dev/disk/by-uuid/5ca9c56c-0372-45e4-8b45-fa8bb5089c3b"; # UUID for /dev/nvme01np2
      preLVM = true;
      keyFile = "/keyfile0.bin";
      allowDiscards = true;
    };
    secrets = { "keyfile0.bin" = "/etc/secrets/initrd/keyfile0.bin"; };
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/5f05ca85-3f1d-4c36-b392-bec176def269";
    fsType = "ext4";
  };

  fileSystems."/boot/efi" = {
    device = "/dev/disk/by-uuid/2DEA-48AF";
    fsType = "vfat";
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/837c185e-1551-4359-9b0b-deb72124aaf7"; }];

  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = "performance";
  hardware.cpu.intel.updateMicrocode = true;
  hardware.nvidiaOptimus.disable = true;
  hardware.acpilight.enable = true;
  hardware.opengl.extraPackages = with pkgs; [
    vaapiIntel
    vaapiVdpau
    libvdpau-va-gl
    intel-media-driver
  ];
  services.tlp.enable = true;
}

