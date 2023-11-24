#!/bin/sh

xrandr --output HDMI2 --right-of eDP1 
xrandr --addmode HDMI2 2560x1440 
xrandr --output HDMI2 --mode 2560x1440

xbacklight -set 30

redshift -P -O 3500
