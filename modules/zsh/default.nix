{ config, lib, pkgs, ... }:

{
  home-manager.users.alternateved.programs = {
    zsh = {
      enable = true;
      enableCompletion = true;
      enableAutosuggestions = true;
      enableSyntaxHighlighting = true;
      history = {
        ignoreDups = true;
        ignoreSpace = false;
        expireDuplicatesFirst = true;
        size = 1000;
        share = true;
        path = "$HOME/.cache/zsh_history";
      };
      defaultKeymap = "emacs";
      initExtra = ''
        export PATH="$HOME/.local/bin:$PATH"
        bindkey -M emacs '^P' history-substring-search-up
        bindkey -M emacs '^N' history-substring-search-down
      '';
      shellAliases = {
        cp = "cp -i"; # Confirm before overwriting something
        df = "df -h"; # Human-readable sizes
        free = "free -m"; # Show sizes in MB
        grep = "grep --color=auto";
        exa = "exa --color=auto --icons --binary --git";
        ls = "exa";
        em = "emacsclient -c";
        ssh = "kitty +kitten ssh";
        nixos-test = "sudo nixos-rebuild test --flake .";
        nixos-switch = "sudo nixos-rebuild switch --flake .";
      };
      sessionVariables = {
        ALTERNATE_EDITOR = "";
        EDITOR = "emacs";
        VISUAL = "emacsclient -c -a ''";
        BROWSER = "firefox";
        CALIBRE_USE_SYSTEM_THEME = "1";
      };
      plugins = [
        {
          name = "zsh-nix-shell";
          file = "nix-shell.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "chisui";
            repo = "zsh-nix-shell";
            rev = "v0.4.0";
            sha256 = "037wz9fqmx0ngcwl9az55fgkipb745rymznxnssr3rx9irb6apzg";
          };
        }
        {
          name = "nix-zsh-completions";
          src = pkgs.fetchFromGitHub {
            owner = "spwhitt";
            repo = "nix-zsh-completions";
            rev = "468d8cf752a62b877eba1a196fbbebb4ce4ebb6f";
            sha256 = "TWgo56l+FBXssOYWlAfJ5j4pOHNmontOEolcGdihIJs=";
          };
        }
        {
          name = "zsh-history-substring-search";
          src = pkgs.fetchFromGitHub {
            owner = "zsh-users";
            repo = "zsh-history-substring-search";
            rev = "4abed97b6e67eb5590b39bcd59080aa23192f25d";
            sha256 = "8kiPBtgsjRDqLWt0xGJ6vBBLqCWEIyFpYfd+s1prHWk=";
          };
        }
      ];
    };
  };

  home-manager.users.alternateved.home.packages = with pkgs; [ exa neofetch ];
}
