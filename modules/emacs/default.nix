{ config, lib, pkgs, ... }: {
  home-manager.users.alternateved.programs = {
    emacs = {
      enable = true;
      package = pkgs.emacsGcc;
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
          ws-butler
          avy
          ace-window
          god-mode

          # Apperance
          ## Modeline
          minions

          all-the-icons
          all-the-icons-dired
          all-the-icons-completion

          # Completion framework
          vertico
          orderless
          marginalia
          consult

          # Org
          org-bullets
          org-roam
          hl-todo
          olivetti

          # Tools
          ## Mail
          org-msg
          ## RSS
          elfeed
          ## Project
          project
          ## Magit
          magit
          ## Terminals
          ### Eshell
          eshell-syntax-highlighting
          eshell-toggle
          ### Vterm
          vterm
          ### Helpful packages
          which-key
          helpful
          rainbow-mode

          # Development
          envrc
          format-all
          ## LSP
          eglot
          ## Completions
          corfu
          ## Checkers
          flymake-diagnostic-at-point
          ## Languages
          nix-mode
          js2-mode
          js2-refactor
          web-mode
          typescript-mode
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
    ## :lang nix
    nixfmt
    ## :lang sh
    shellcheck
    ## :lang javascript, webm json
    jq
    nodejs
    nodePackages.npm
  ];
}
