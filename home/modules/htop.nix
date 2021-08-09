{
  programs.htop = {
    enable = true;
    settings = {
      color_scheme = 0;
      enable_mouse = true;
      show_program_path = false;
      sort_direction = 1;
      sort_key = 2;
      tree_view = true;
      vim_mode = true;

      left_meters = [ "AllCPUs" ];
      left_meter_modes = [ 1 ];
      right_meters = [ "Memory" "Swap" "Tasks" "LoadAverage" "Uptime" ];
      left_emter_modes = [ 1 1 2 2 2 ];
    };
  };
}
