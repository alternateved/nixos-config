{
  services.picom = {
    enable = true;
    vSync = true;
    fade = false;
    shadow = false;
    blur = true;
    shadowExclude = [ "class_g = 'Firefox Developer Edition' && argb" ];
    blurExclude = [ "class_g = 'Firefox Developer Edition' && argb" ];
    backend = "glx";
    experimentalBackends = true;
    extraOptions = ''
      use-damage = true;
      blur-strength = 12;
    '';
  };
}
