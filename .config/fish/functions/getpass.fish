function getpass

    set chosen (op list items | jq --raw-output '.[].overview.title' | fzf)
    echo Chose: $chosen
    op get item "$chosen"
end

