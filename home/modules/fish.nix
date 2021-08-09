{
  programs = {
    fish = {
      enable = true;
      shellAliases = {
        cp = "cp -i"; # Confirm before overwriting something
        df = "df -h"; # Human-readable sizes
        free = "free -m"; # Show sizes in MB
        ls = "ls --color=auto"; # colored output
        emacs = "devour emacsclient -c -a";
        mpv = "devour mpv";
        zathura = "devour zathura";
        sxiv = "devour sxiv";
        vifm = "~/.config/vifm/scripts/vifmrun";
      };
    };
    starship = {
      enable = true;
      enableBashIntegration = true;
      enableFishIntegration = true;
    };
  };
}
