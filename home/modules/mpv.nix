{
  programs.mpv = {
    enable = true;
    config = ''
      # Options
      ### For more information, please refer to https://mpv.io/manual/master.
      # Track Selection
      track-auto-selection=yes
      # Playback Control
      hr-seek=absolute
      # Program Behavior
      save-position-on-quit
      load-stats-overlay=no
      # Video
      vo=gpu
      framedrop=vo
      ### If you want to use software decode, comment the line below and uncomment the next
      hwdec=auto-safe
      #hwdec=no
      video-aspect-override=-1
      video-output-levels=auto
      vd-lavc-dr=yes
      ### Discard part of detections and calculations to decode faster
      #vd-lavc-fast=yes
      ### Force to use more threads to decode
      #vd-lavc-threads=16
      ### Improve pipelining and prevent missed vsyncs but increases visible latency
      #swapchain-depth=3
      # Audio
      audio-pitch-correction=yes
      audio-delay=0
      mute=no
      ### Enable exclusive audio output mode, only mpv will be able to output audio
      #audio-exclusive=yes
      ### Force a plain stereo downmix
      #audio-channels=stereo
      gapless-audio=weak
      audio-file-auto=fuzzy
      audio-file-paths=**
      # Subtitle
      sub-ass-force-margins=yes
      sub-use-margins=yes
      sub-auto=fuzzy
      sub-font=FZZhunYuan-M02
      sub-font-size=45
      sub-spacing=2
      ### Use fontconfig
      #sub-font-provider=fontconfig
      # Window
      keep-open=no
      image-display-duration=inf
      force-window=immediate
      snap-window
      #ontop
      autofit-larger=1280
      cursor-autohide=1000
      cursor-autohide-fs-only
      stop-screensaver
      # Disc Devices
      # Equalizer
      # Demuxer
      ### Set a larger buffer ahead for demuxer
      #demuxer-max-bytes=300000
      demuxer-readahead-secs=10
      ### Prefetch next playlist entry while playback of the current entry is ending
      #prefetch-playlist=yes
      ### Force enabling seeking even the player thinks the media is not seekable
      #force-seekable=yes
      # Input
      no-input-default-bindings
      # OSC
      osc=no
      # OSD
      no-osd-bar
      osd-border-size=3
      # Screenshot
      screenshot-format=png
      screenshot-template=mpv-screenshot-%ty%tm%td-%n
      screenshot-directory=~~desktop/
      # Software Scaler
      # Audio Resampler
      ### Enable normalization if surround audio is downmixed to stereo
      #audio-normalize-downmix=yes
      # Terminal
      # TV
      # Cache
      # Network
      # DVB
      # ALSA audio output options
      # GPU renderer options
      opengl-pbo
      ### Set a larger dither matrix size, slow down mpv startup slightly
      #dither-size-fruit=7
      ### Better dither kernal, requires compute shader and large amount of memory, comment the line below and uncomment the next
      dither=fruit
      temporal-dither
      ### Better GPU context backend, Direct3D11 through the OpenGL ES translation layer ANGLE
      #gpu-context=angle
      gpu-api=d3d11
      ### Set your own target peak for display
      target-peak=100
      #target-peak=300
      tone-mapping=hable
      ### HDR tone mapping related parameters
      #tone-mapping-param=0.3
      hdr-compute-peak
      #tone-mapping-desaturate=0.5
      ### Automatically select the ICC display profile currently specified by the display settings of the operating system
      #icc-profile-auto
      ### Set a larger 3D LUT size, slow down mpv startup slightly
      #icc-3dlut-size=256x256x256
      blend-subtitles=no
      # Miscellaneous
      priority=high
      autoload-files=yes
      # Audio Output Drivers
      # Video Output Drivers
      # Video Filters
      # Audio Filters
      ### High quality pitch correction with librubberband, better than scaletempo in some ways
      #af=rubberband
      # Encoding
    '';
  };
}
