{ pkgs, ... }: {
  programs.neovim = {
    enable = true;
    package = pkgs.neovim-unwrapped;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    extraConfig = ''
      if $TERM ==# 'linux'
         " commands
         colorscheme slate
      else
         " more commands
         colorscheme xresources
      endif

        lua require("init")
    '';
  };

  xdg.configFile."nvim/lua" = {
    source = ../../config/nvim;
    recursive = true;
  };
}
