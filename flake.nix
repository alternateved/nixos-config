{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    home-manager.url = "github:rycee/home-manager/master";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    xmonad.url = "github:xmonad/xmonad";
    xmonad-contrib.url = "github:xmonad/xmonad-contrib";
  };

  outputs = { nixpkgs, nixos-hardware, home-manager, emacs-overlay, xmonad
    , xmonad-contrib, ... }: {
      nixosConfigurations = {
        teishi = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./hosts/teishi
            home-manager.nixosModules.home-manager
            {
              nixpkgs = {
                config = { allowUnfree = true; };
                overlays = [
                  emacs-overlay.overlay
                  xmonad.overlay
                  xmonad-contrib.overlay
                  (import ./overlays)
                ];
              };
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.alternateved = import ./modules/extended.nix;
            }
          ];
        };
        ragi = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          modules = [
            ./hosts/ragi
            nixos-hardware.nixosModules.raspberry-pi-4
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.alternateved = import ./modules/minimal.nix;
            }
          ];
        };
      };
    };
  # });
}
