function _tide_item_kubectx
    set_color bryellow
    set -l current_kubectx (kubectx -c 2> /dev/null)
    test -n "$current_kubectx"; and echo -n 'ﴱ ' $current_kubectx
end
