# Defined in /tmp/fish.jk5fS3/sw.fish @ line 2
function sw --description 'Start Windows Environment'
    # Setup X11 virtual monitors, if not done already
    if xrandr --listactivemonitors | not grep -q 'default~2'
        xrandr --setmonitor default~1 1920/254x1080/286+0+0 default
        xrandr --setmonitor default~2 1920/255x1080/286+1920+268 none
    end

    xrdb -load $HOME/.Xresources

    if set -q DBUS_SESSION_BUS_ADDRESS
        fenv "eval `dbus-launch --sh-syntax`"
        echo "D-Bus per-session daemon address is: $DBUS_SESSION_BUS_ADDRESS"
    end

    fenv "eval `gnome-keyring-daemon -r -d -c secrets,ssh,pkcs11`"
    # pycharm > /dev/null 2>&1 &
    urxvt &
    emacs &
end
