{ pkgs, nix, nixpkgs, config, lib, ... }: {

  imports = [
    ../../modules/alacritty
    ../../modules/direnv
    # ../../modules/emacs
    ../../modules/fzf
    ../../modules/git
    ../../modules/htop
    # ../../modules/mail
    # ../../modules/nnn
    ../../modules/neovim
    ../../modules/starship/lambda.nix
    # ../../modules/theme
    ../../modules/zsh
  ];

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      (iosevka-bin.override { variant = "aile"; })
      (iosevka-bin.override { variant = "etoile"; })
      (nerdfonts.override { fonts = [ "Iosevka" ]; })
    ];
  };

  users = { users.alternateved = { home = /Users/alternateved; }; };

  environment.systemPackages = with pkgs; [ git wget ];

  nix = {
    gc = {
      automatic = true;
      options = "--delete-older-than 8d";
    };
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  system.stateVersion = 4;
}
