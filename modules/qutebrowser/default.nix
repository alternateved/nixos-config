{
  programs.qutebrowser = {
    enable = true;
    extraConfig = builtins.readFile ../../config/qutebrowser/config.py;
  };
}
