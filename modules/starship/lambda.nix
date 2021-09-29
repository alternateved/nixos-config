{
  home-manager.users.alternateved.programs = {
    starship = {
      enable = true;
      enableZshIntegration = true;
      settings = {
        command_timeout = 1000;
        add_newline = false;
        line_break = { disabled = true; };
        character = {
          success_symbol = "[λ](bold gray)";
          error_symbol = "[λ](bold red)";
          vicmd_symbol = "[V](bold green)";
        };
        directory = {
          style = "bold green";
          truncation_length = 3;
          truncation_symbol = "…/";
          truncate_to_repo = true;
          read_only = " ";
        };
        git_branch = { style = "bold purple"; };
        git_status = { style = "bold purple"; };
        package = { disabled = true; };
      };
    };
  };

}
