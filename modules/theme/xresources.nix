let colors = import ./colors.nix;
in
{
  home-manager.users.alternateved.xresources.extraConfig = ''
     Xft.antialias: 1
     Xft.hinting: 1
     Xft.autohint: 0
     Xft.hintstyle: hintslight
     Xft.rgba: rgb
     Xft.lcdfilter: lcddefault

     Sxiv.background: ${colors.background}
     Sxiv.foreground: ${colors.foreground}
     Sxiv.font: JuliaMono:style=regular:size=11

     Xmessage.font: JuliaMono:pixelsize=12

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
