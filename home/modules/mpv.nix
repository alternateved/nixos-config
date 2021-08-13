{
  programs.mpv = {
    enable = true;
    config = {
      profile = "gpu-hq";
      vo = "gpu";
      # gpu-api = "vulkan";
      hwdec = "auto-copy";
      force-window = true;
      ytdl-format = "bestvideo+bestaudio";
      cache-default = 4000000;
      save-position-on-quit = true;
    };
  };
}
