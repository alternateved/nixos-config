let colors = import ../../home/theme/colors.nix;
in {
      services.xserver.displayManager = {
        autoLogin.enable = true;
        autoLogin.user = "alternateved";
        lightdm.greeters.mini = {
          enable = true;
          user = "alternateved";
          extraConfig = ''
            [greeter-theme]
            background-image = "";
            background-color = ${colors.background}
            text-color = ${colors.foreground}
            password-background-color = ${colors.black}
            window-color = ${colors.background}
            border-color = ${colors.foreground}
          '';
        };
      };
}
