# Defined in /tmp/fish.P6x2Se/sw.fish @ line 2
function sw --description 'Start Windows Environment'
    xrdb -load $HOME/.Xresources
    fenv dbus-launch
    fenv gnome-keyring-daemon -r -d -c secrets,ssh,pkcs11
    pycharm > /dev/null 2>&1 &
    urxvt &
    emacs &
end
