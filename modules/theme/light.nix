{ pkgs, ... }:
let colors = import ./colors.nix;
in {
  home-manager.users.alternateved.gtk = {
    theme = {
      package = pkgs.materia-theme;
      name = "Materia-light-compact";
    };
    gtk3.extraConfig = { gtk-application-prefer-dark-theme = 0; };
    iconTheme = {
      package = pkgs.tela-icon-theme;
      name = "Tela";
    };
  };
  home-manager.users.alternateved.xresources.extraConfig = ''
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
     *.color8: ${colors.white}

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
     *.color7: ${colors.bright-black}
     *.color15: ${colors.bright-white}
  '';
}
