#!/usr/bin/env bash

wid=$1
class=$2
instance=$3
title=$(xdotool getwindowname $wid)

case $class in
  Skype|Sxiv|flameshot)
	  echo "state = floating"
	  echo "center = on"
	  ;;
  Emacs|Zathura)
	  echo "state = tiled"
	  ;;
  Thunderbird|Signal|discord)
	  echo "desktop = Communication"
	  ;;
  mpv|Spotify)
	  echo "desktop = Media"
	  ;;
esac

case $title in
  terrm|monitorr)
	  echo "state = floating"
	  echo "center = on"
	  ;;
  mixerr)
	  echo "state = floating"
	  echo "rectangle = 1000x300+0+0"
	  echo "center = on"
	  ;;
  spotiff)
	  echo "state = floating"
	  echo "rectangle = 1100x500+0+0"
	  echo "center = on"
	  ;;
esac
