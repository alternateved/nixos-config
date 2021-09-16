{ pkgs, ... }: {
  services.mbsync.enable = true;
  home.packages = with pkgs; [ mu isync msmtp rbw pinentry ];
}
