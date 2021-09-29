{ pkgs, ... }: {
  home-manager.users.alternateved.programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    extraConfig = ''
      lua require("init")
    '';
  };

  home-manager.users.alternateved.xdg.configFile."nvim/lua" = {
    source = ../../config/nvim;
    recursive = true;
  };
}
