{ pkgs, ... }: {
  programs.neovim = {
    enable = true;
    package = pkgs.neovim-unwrapped;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    extraConfig = ''
      lua require("init")
    '';
  };

  xdg.configFile."nvim/lua" = {
    source = ../../config/nvim;
    recursive = true;
  };
}
