{ pkgs, ... }:
let colors = import ./colors.nix;
in {
  dconf.enable = false;
  gtk = {
    enable = true;
    theme = {
      package = pkgs.materia-theme;
      name = "Materia-dark-compact";
    };
    gtk3.extraConfig = { gtk-application-prefer-dark-theme = 1; };
    iconTheme = {
      package = pkgs.papirus-icon-theme;
      name = "Papirus-Dark";
    };
    font = {
      package = null;
      name = "Iosevka Aile Medium 11";
    };
  };
  qt = {
    enable = true;
    platformTheme = "gtk";
  };
  home.packages = with pkgs; [ libsForQt5.qtstyleplugins ];
  xsession.pointerCursor = {
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ";
    size = 16;
  };
  fonts.fontconfig.enable = true;

  xresources.extraConfig = ''
     Xft.antialias: 1
     Xft.hinting: 1
     Xft.autohint: 0
     Xft.hintstyle: hintslight
     Xft.rgba: rgb
     Xft.lcdfilter: lcddefault

     Sxiv.background: ${colors.background}
     Sxiv.foreground: ${colors.foreground}
     Sxiv.font: Iosevka Nerd Font Mono:style=regular:size=11

     Xmessage.font: Iosevka Nerd Font Mono:pixelsize=12

     *.foreground: ${colors.foreground}
     *.background: ${colors.background}

    *.cursorColor: ${colors.foreground}

     ! Black
     *.color0: ${colors.black}
     *.color8: ${colors.bright-black}

     ! Red
     *.color1: ${colors.red}
     *.color9: ${colors.bright-red}

     ! Green
     *.color2: ${colors.green}
     *.color10: ${colors.bright-green}

     ! Yellow
     *.color3: ${colors.yellow}
     *.color11: ${colors.bright-yellow}

     ! Blue
     *.color4: ${colors.blue}
     *.color12: ${colors.bright-blue}

     ! Magenta
     *.color5: ${colors.magenta}
     *.color13: ${colors.bright-magenta}

     ! Cyan
     *.color6: ${colors.cyan}
     *.color14: ${colors.bright-cyan}

     ! White
     *.color7: ${colors.white}
     *.color15: ${colors.bright-white}
  '';
}
