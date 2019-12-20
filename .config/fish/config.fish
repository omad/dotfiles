
if type -q conda
   # >>> conda initialize >>>
   # !! Contents within this block are managed by 'conda init' !!
   eval conda "shell.fish" "hook" $argv | source
   # <<< conda initialize <<<
end


if test -f /opt/Modules/v4.3.0/init/fish
    source /opt/Modules/v4.3.0/init/fish
    source /opt/Modules/v4.3.0/init/fish_completion
end
