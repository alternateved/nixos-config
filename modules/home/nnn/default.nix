{ pkgs, ... }:

let nnn = pkgs.nnn.override { withNerdIcons = true; };
in {
  home.packages = with pkgs; [ nnn ];
  home.sessionVariables = {
    BLK = "04";
    CHR = "04";
    DIR = "04";
    EXE = "00";
    REG = "00";
    HARDLINK = "00";
    SYMLINK = "06";
    MISSING = "00";
    ORPHAN = "01";
    FIFO = "0F";
    SOCK = "0F";
    OTHER = "02";
    NNN_FCOLORS =
      "$BLK$CHR$DIR$EXE$REG$HARDLINK$SYMLINK$MISSING$ORPHAN$FIFO$SOCK$OTHER";
  };
}
