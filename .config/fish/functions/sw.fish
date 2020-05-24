function sw --description 'Start Windows Environment'
    xrdb -load $HOME/.Xresources
    fenv "eval `dbus-launch`"
    fenv "eval `gnome-keyring-daemon -r -d -c secrets,ssh,pkcs11`"
    pycharm > /dev/null 2>&1 &
    urxvt &
    emacs &
end
