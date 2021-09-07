{ pkgs, ... }: {
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    extraConfig = ''
      lua require("init")
    '';
  };

  xdg.configFile."nvim/lua" = {
    source = ../../../config/nvim;
    recursive = true;
  };
}
