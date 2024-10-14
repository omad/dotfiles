function __complete_s5cmd
    set -lx COMP_LINE (commandline -cp)
    #    test -z (commandline -ct)
    #    and set COMP_LINE "$COMP_LINE "
    #    echo (date) (count $COMP_LINE) >> /tmp/completion_debugging
    #    echo (date) "Given: **" $COMP_LINE "**" >> /tmp/completion_debugging

    set -lx COMP_LINE (string split " " $COMP_LINE)

    #    echo (date) "Given: **" $COMP_LINE "**" >> /tmp/completion_debugging

    $COMP_LINE --generate-bash-completion
end
complete -f -c s5cmd -a "(__complete_s5cmd)"
