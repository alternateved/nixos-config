{
  programs = {
    fish = {
      enable = true;
      shellAliases = {
        cp = "cp -i"; # Confirm before overwriting something
        df = "df -h"; # Human-readable sizes
        free = "free -m"; # Show sizes in MB
        ls = "ls --color=auto"; # colored output
        em = "devour emacsclient -c";
        mpv = "devour mpv";
        zathura = "devour zathura";
        calibre = "CALIBRE_USE_DARK_PALETTE=1 calibre";
      };
    };
    starship = {
      enable = true;
      enableFishIntegration = true;
    };
  };
}