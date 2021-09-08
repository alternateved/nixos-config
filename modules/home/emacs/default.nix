{ config, lib, pkgs, ... }: {
  programs = {
    emacs = {
      enable = true;
      package = pkgs.emacsGcc;
      extraPackages = (epkgs: [ epkgs.vterm ]);
    };
  };
  services = { emacs.enable = true; };
  home.packages = with pkgs; [
    ## Doom dependencies
    git
    coreutils
    binutils

    ## Optional dependencies
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
    nodePackages.bash-language-server
    ## :lang javascript, webm json
    jq
    nodejs
    html-tidy
    nodePackages.npm
    nodePackages.stylelint
    nodePackages.js-beautify
    nodePackages.typescript-language-server
    nodePackages.vscode-json-languageserver-bin
    nodePackages.vscode-html-languageserver-bin
  ];
}
