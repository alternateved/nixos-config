{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:rycee/home-manager/master";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    xmonad.url = "github:xmonad/xmonad";
    xmonad-contrib.url = "github:xmonad/xmonad-contrib";
  };

  outputs =
    { nixpkgs, home-manager, emacs-overlay, xmonad, xmonad-contrib, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
        overlays = [
          emacs-overlay.overlay
          xmonad.overlay
          xmonad-contrib.overlay
          (import ./overlays)
        ];
      };
    in {
      # devShell.${system} = import ./shell.nix {
      # };

      nixosConfigurations = {
        nixos = nixpkgs.lib.nixosSystem {
          inherit pkgs system;
          modules = [
            ./hosts/teishi
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = { inherit pkgs; };
              home-manager.users.alternateved = import ./modules;
            }
          ];
        };
      };
    };
}
