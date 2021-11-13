{
  home-manager.users.alternateved.programs = {
    starship = {
      enable = true;
      enableZshIntegration = true;
      settings = {
        command_timeout = 5000;
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
          read_only = " ro";
        };
        git_branch = { style = "bold purple"; symbol = ""; };
        git_status = {
          style = "bold purple";
          ahead = ">";
          behind = "<";
          diverged = "<>";
          renamed = "r";
          deleted = "x";
        };
        git_commit = { tag_symbol = " tag "; };
        nix_shell = { style = "bold blue"; symbol = "nix "; format = "via [$symbol]($style)"; };
        nodejs = { symbol = "nodejs "; };
        package = { symbol = "pkg "; };
        python = { symbol = "python "; };
        ruby = { symbol = "ruby "; };
        rust = { symbol = "rust"; };
        lua = { symbol = "lua "; };
      };
    };
  };

}
