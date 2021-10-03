{ config, lib, pkgs, ... }: {
  home-manager.users.alternateved.programs = {
    emacs = {
      enable = true;
      package = pkgs.emacsPgtkGcc;
      extraPackages = (epkgs:
        with epkgs; [
          # Package managements
          use-package
          auto-compile

          # Cache
          gcmh
          no-littering

          # Editor
          smartparens
          rainbow-delimiters
          undo-fu
          undo-fu-session

          # Apperance
          mood-line
          hide-mode-line
          all-the-icons
          all-the-icons-dired
          all-the-icons-completion

          # Evil
          evil
          evil-collection
          evil-org
          evil-surround
          evil-nerd-commenter
          better-jumper

          # Keybindings
          general

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
          ## Projectile
          projectile
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
          flycheck
          format-all
          ## LSP
          lsp-mode
          lsp-ui
          ## Company
          company
          company-box
          ## Languages
          nix-mode
          js2-mode
          js2-refactor
          web-mode
          typescript-mode
          haskell-mode
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
    nodePackages.prettier
    nodePackages.typescript-language-server
    nodePackages.vscode-json-languageserver-bin
    nodePackages.vscode-html-languageserver-bin
  ];
}
