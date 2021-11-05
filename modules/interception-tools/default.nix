{ config, pkgs, ... }: {
  environment.systemPackages = with pkgs; [ interception-tools ];
  services.interception-tools = let
    dualFunctionKeysConfig = pkgs.writeText "dual-function-keys.yaml" ''
      TIMING:
        TAP_MILLISEC: 200
        DOUBLE_TAP_MILLISEC: 150

      MAPPINGS:
        - KEY: KEY_ENTER
          TAP: KEY_ENTER
          HOLD: KEY_RIGHTCTRL
    '';
  in {
    enable = true;
    plugins = [ pkgs.interception-tools-plugins.dual-function-keys ];
    udevmonConfig = ''
      - JOB: "${pkgs.interception-tools}/bin/intercept -g $DEVNODE | ${pkgs.interception-tools-plugins.dual-function-keys}/bin/dual-function-keys -c ${dualFunctionKeysConfig} | ${pkgs.interception-tools}/bin/uinput -d $DEVNODE"
        DEVICE:
          EVENTS:
            EV_KEY: [KEY_ENTER]
    '';
  };
}
