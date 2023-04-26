function cat --description 'Magic cat + mdcat + bat'
    set -l md_exts md markdown txt
    set -l img_exts png gif jpg jpeg
    set -l nb_exts ipynb

    if not isatty stdout
        command cat $argv
    else if contains (get_ext $argv) $md_exts
        mdcat --paginate $argv
    else if contains (get_ext $argv) $nb_exts; and type -q nbpreview
        nbpreview $argv
    else if contains (get_ext $argv) $img_exts; and test $TERM = xterm-kitty
        command kitty +kitten icat $argv
    else
        command bat $argv
    end
end
