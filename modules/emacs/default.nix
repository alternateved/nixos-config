{ config, lib, pkgs, ... }: {
  home-manager.users.alternateved.programs = {
    emacs = {
      enable = true;
      package = pkgs.emacsGcc;
      overrides = self: super: {
        org = self.elpaPackages.org;
      };
      extraPackages = (epkgs:
        with epkgs; [
          # Package managements
          use-package
          auto-compile

          # Cache
          gcmh
          no-littering

          # Editor
          rainbow-delimiters
          avy
          ace-window
          god-mode
          dot-mode
          multiple-cursors
          expand-region

          # Apperance
          minions
          kaolin-themes
          all-the-icons
          all-the-icons-dired
          all-the-icons-completion

          # Completion framework
          vertico
          orderless
          marginalia
          consult
          embark
          embark-consult

          # Org
          org
          org-bullets
          org-roam
          hl-todo
          olivetti

          # Tools
          org-msg
          elfeed
          project
          magit
          diff-hl
          eshell-syntax-highlighting
          eshell-toggle
          vterm
          which-key
          helpful
          rainbow-mode

          # Development
          envrc
          dumb-jump
          eglot
          consult-eglot
          corfu
          cape

          ## Languages
          flymake-shellcheck
          nix-mode
          js2-mode
          js2-refactor
          web-mode
          typescript-mode
          json-mode
          haskell-mode
          hindent
          toml-mode
          yaml-mode
          lua-mode
          markdown-mode
        ]);
    };
  };
  home-manager.users.alternateved.services = { emacs.enable = true; };
  home-manager.users.alternateved.home.packages = with pkgs; [
    ## Dependencies
    git
    coreutils
    binutils
    fd
    ripgrep
    zstd

    ## Module dependencies
    ### :checkers spell, grammar
    (aspellWithDicts (ds: with ds; [ en pl ]))
    languagetool
    ### :term vterm
    libvterm
    gnumake
    cmake
    gcc
    ### :tty
    xclip
    ### :lang markdown
    pandoc
    ### :lang org
    graphviz
    sqlite
    ### :ui treemacs
    python3Minimal
    ## :lang sh
    shellcheck
    ## :lang javascript, webm json
    jq
    nodejs
    nodePackages.npm
  ];
}
