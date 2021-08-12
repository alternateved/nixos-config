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
  services.flameshot.enable = true;
}
