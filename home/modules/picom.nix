{
  services.picom = {
    enable = true;
    vSync = true;
    backend = "glx";
    experimentalBackends = true;

    ### FADE ###
    fade = false;
    fadeDelta = 10;
    fadeSteps = [ (3.0e-2) (3.0e-2) ];
    fadeExclude = [ "class_g = 'slop'" ];
    no-fading-openclose = false;
    no-fading-destroyed-argb = true;

    ### SHADOW ###
    shadow = false;
    shadow-radius = 7;
    shadow-color = "#000000";
    shadowOffsets = [ (-7) (-7) ];
    shadowOpacity = 0.75;
    shadowExclude = [
      "class_g = 'Firefox Developer Edition' && argb"
      "fullscreen"
      "! name~=''"
      "!WM_CLASS:s"
      "_GTK_FRAME_EXTENTS@:c"
      "_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
    ];

    ### BLUR ###
    blur = {
      method = "kawase";
      strength = "7.0";
    };
    blur-background = false;
    blur-background-frame = false;
    blur-background-fixed = false;
    blurExclude = [
      "class_g = 'Firefox Developer Edition' && argb"
      "_GTK_FRAME_EXTENTS@:c"
    ];

    ### Animations ###
    transition-length = 150;
    transition-pow-x = 0.1;
    transition-pow-y = 0.1;
    transition-pow-w = 0.1;
    transition-pow-h = 0.1;
    size-transition = true;

    ### TRANSPARENCY / OPACITY ###
    frame-opacity = 1.0;
    inactive-opacity-override = false;
    active-opacity = 1.0;
    focus-exclude = [ "class_g ?= 'rofi'" "class_g ?= 'Steam'" ];

    ### OTHER ###
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
  };
}
