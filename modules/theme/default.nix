{ pkgs, ... }: {
  imports = [ ./gtk.nix ./qt.nix ./xresources.nix ];
  xsession.pointerCursor = {
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ";
    size = 16;
  };
  fonts.fontconfig.enable = true;
}
