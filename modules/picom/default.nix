{
  home-manager.users.alternateved.services.picom = {
    enable = true;
    vSync = true;
    backend = "glx";
    experimentalBackends = true;

    ### BLUR ###
    blur = true;
    blurExclude = [
      "class_g = 'Firefox Developer Edition' && argb"
      "_GTK_FRAME_EXTENTS@:c"
    ];

    ### FADE ###
    fade = false;
    fadeDelta = 10;
    fadeSteps = [ (3.0e-2) (3.0e-2) ];
    fadeExclude = [ "class_g = 'slop'" ];

    ### SHADOW ###
    shadow = false;
    shadowOffsets = [ (-7) (-7) ];
    shadowOpacity = "0.75";
    shadowExclude = [
      "class_g = 'Firefox Developer Edition' && argb"
      "fullscreen"
      "! name~=''"
      "!WM_CLASS:s"
      "_GTK_FRAME_EXTENTS@:c"
      "_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
    ];
    extraOptions = ''
      mark-wmwin-focused = true;
      mark-ovredir-focused = true;
      detect-rounded-corners = true;
      detect-client-opacity = true;

      unredir-if-possible-exclude = [ ];
      detect-transient = true;
      detect-client-leader = true;

      invert-color-include = [ ];
      glx-no-stencil = true;
      use-damage = false;
      transparent-clipping = false;
    '';
  };
}
