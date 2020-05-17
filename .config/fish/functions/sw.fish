# Defined in /tmp/fish.13SojN/sw.fish @ line 2
function sw --description 'Start Windows Environment'
    xrdb -load $HOME/.Xresources
    pycharm > /dev/null 2>&1 &
    urxvt &
    emacs &
end
