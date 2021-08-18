#!/run/current-system/sw/bin/bash
cd ~/.config/xmobar || exit
killall xmobar
nix-shell --pure --run "xmobar --recompile"
xmonad --restart
