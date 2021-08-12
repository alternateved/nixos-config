{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:rycee/home-manager/master";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { nixpkgs, home-manager, emacs-overlay, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
        overlays = [
          emacs-overlay.overlay
          # (import ./overlays)
        ];
      };
    in {
      # devShell.${system} = import ./shell.nix {
      # };

      nixosConfigurations = {
        nixos = nixpkgs.lib.nixosSystem {
          inherit system;

          modules = [
            ./system
            home-manager.nixosModules.home-manager
            # {
            #   nixpkgs.overlays = [
            #     emacs-overlay.overlay
            #     # xmonad.overlay
            #     # xmonad-contrib.overlay
            #     # xmonad-extras.overlay
            #     # (import ./overlays)
            #   ];
            # }
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = { inherit pkgs; };
              home-manager.users.alternateved = import ./home;
            }
          ];
        };
      };
    };
}
