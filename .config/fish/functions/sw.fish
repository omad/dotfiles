# Defined in /tmp/fish.Xg8mzE/sw.fish @ line 2
function sw --description 'Start Windows Environment'
    xrandr --setmonitor default~1 1920/254x1080/286+0+0 default
    xrandr --setmonitor default~2 1920/255x1080/286+1920+268 none

    xrdb -load $HOME/.Xresources
    fenv "eval `dbus-launch`"
    fenv "eval `gnome-keyring-daemon -r -d -c secrets,ssh,pkcs11`"
    # pycharm > /dev/null 2>&1 &
    urxvt &
    emacs &
end
