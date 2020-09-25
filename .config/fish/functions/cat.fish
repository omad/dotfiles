function cat --description 'Magic cat + mdcat + bat'
    set -l extrs md markdown txt

    if not isatty stdout
        command cat $argv
    else if contains (get_ext $argv) $exts
        mdcat --paginate $argv
    else
        command bat $argv
    end
end
