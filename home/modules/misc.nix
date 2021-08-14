{
  programs = {
    direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
        enableFlakes = true;
      };
    };
    fzf = {
      enable = true;
      enableZshIntegration = true;
    };
  };
  xdg = {
    enable = true;
    userDirs.enable = true;
  };
  services.flameshot.enable = true;
}
