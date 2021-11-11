{ pkgs, config, ... }:
{

  imports = [
    # ../alacritty
    ../../modules/kitty
    ../../modules/direnv
    ../../modules/fzf
    # ../git
    ../../modules/htop
    ../../modules/nnn
    # ../neovim
    # ../starship/lambda.nix
    # ../theme
    # ../zsh
  ];

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      alegreya
      alegreya-sans
      emacs-all-the-icons-fonts
      (nerdfonts.override {
        fonts = [ "JetBrainsMono" ];
      })
    ];

    networking.hostName = "yotsugi";

    system.defaults = {
      NSGlobalDomain = {
        AppleInterfaceStyle = "Auto"; # Dark mode
      };
      dock = {
        autohide = true;
        show-recents = false;
      };
      finder = {
        AppleShowAllExtensions = true;
      };
      trackpad.Clicking = true;
    };

    services.nix-daemon = {
      enable = true;
    };

    nix = {
      gc = {
        automatic = true;
        options = "--delete-older-than 8d";
      };
      package = pkgs.nixUnstable;
      useSandbox = false;
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
    };
    system.stateVersion = 4;
  }
