#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then







    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then







    xrdb -merge "$userresources"

fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

#DONT FORGET TO PUT AMPERSAND (&)!!!!!!!!!!!
#wmname LG3D
xss-lock -- xscreensaver-command -lock &
setxkbmap -layout us,th -option grp:lctrl_lshift_toggle
setxkbmap -option caps:escape
xxkb &
megasync &
dropbox &
#nm-applet &
xbindkeys
xset s 3600 3600
xset dpms 3610 3610 3610
# udisksvm -a &
udiskie --tray &
compton --config ~/.compton.conf -b
feh --bg-scale ~/Pictures/yosemite.png &
stalonetray &
mpd ~/.mpdconf/mpd.conf
#copyq &
imwheel
xinput --set-prop 11 'libinput Accel Speed' 0.5 &
xscreensaver &
redshift &
exec xmonad
