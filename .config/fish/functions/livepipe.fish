function livepipe --description 'Interactive live command runner using fzf; auto snapshot stdin if piped'
    # Detect whether stdin is a pipe or a TTY
    # breakpoint
    set -l has_pipe 0
    if not test -t 0
        set has_pipe 1
    end

    set -l tmp_in ""
    if test $has_pipe -eq 1
        set tmp_in (mktemp -t livepipe.in.XXXXXX)
        cat > $tmp_in
    end

    # Build reload command
    # {q} is replaced by fzf with the current query
    if test $has_pipe -eq 1
        set --function reload_cmd "reload:bash -lc {q} < $tmp_in 2>&1 || :"
        # set --function reload_cmd "reload:{q} < $tmp_in 2>&1 || :"
        set --function header "stdin: SNAPSHOT (piped)"
    else
        set --function reload_cmd "reload:bash -lc {q} 2>&1 || :"
        # set --function reload_cmd "reload:{q} 2>&1 || :"
        set --function header "stdin: none"
    end

    if test -n "$argv"
        set --function default_query $argv
    else
        set default_query cat
        echo no arg, setting default query
    end

    set -l transform_query "echo {q} | sed -e 's/yq/yq -C/' "

                # --tac \ not required with layout=reverse
                # --header "$header" \
    fzf --disabled --ansi --border \
                --prompt 'cmd> ' \
                --info hidden \
                --margin 1,5% \
                --info-command "echo $header" \
                --ghost "Type command to run here..." \
                --layout=reverse \
                --no-sort \
                --print-query \
                --bind "start:$reload_cmd" \
                --bind "change:$reload_cmd" \
                --query "$default_query"
                # --bind "transform-query($transform_query)" \

    # Cleanup
    if test -n "$tmp_in"
        rm -f "$tmp_in"
    end
end
