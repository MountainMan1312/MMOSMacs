# .xinitrc for MMOSMacs
# This script is executed when running the `startx` command.


# Make Java applications aware this is a non-reparenting window manager
export _JAVA_AWT_WM_NONREPARENTING=1


# Set default cursor
xsetroot -cursor_name left_ptr


# Set keyboard repeat rate
xset r rate 200 60


# Start emacs
exec dbus-launch --exit-with-session emacs
