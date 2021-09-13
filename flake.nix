{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    home-manager.url = "github:rycee/home-manager/master";
    nur.url = "github:nix-community/NUR";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    xmonad.url = "github:xmonad/xmonad";
    xmonad-contrib.url = "github:xmonad/xmonad-contrib";
  };

  outputs = { nixpkgs, nixos-hardware, home-manager, nur, emacs-overlay, xmonad
    , xmonad-contrib, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
        overlays = [
          nur.overlay
          emacs-overlay.overlay
          xmonad.overlay
          xmonad-contrib.overlay
          (import ./overlays)
        ];
      };
    in {
      nixosConfigurations = {
        teishi = nixpkgs.lib.nixosSystem {
          inherit pkgs system;
          modules = [
            ./hosts/teishi
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.alternateved =
                import ./modules/home/extended.nix;
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
              home-manager.users.alternateved =
                import ./modules/home/minimal.nix;
            }
          ];
        };
      };
    };
  # });
}
