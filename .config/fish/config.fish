
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

set -gx MANPAGER 'less -X'

# Colorize man
set MANROFFOPT '-c'
set LESS_TERMCAP_mb (tput bold; tput setaf 2)
set LESS_TERMCAP_md (tput bold; tput setaf 6)
set LESS_TERMCAP_me (tput sgr0)
set LESS_TERMCAP_so (tput bold; tput setaf 3; tput setab 4)
set LESS_TERMCAP_se (tput rmso; tput sgr0)
set LESS_TERMCAP_us (tput smul; tput bold; tput setaf 7)
set LESS_TERMCAP_ue (tput rmul; tput sgr0)
set LESS_TERMCAP_mr (tput rev)
set LESS_TERMCAP_mh (tput dim)

# pyenv
if test -d $HOME/.pyenv
	set -x PYENV_ROOT $HOME/.pyenv
	set -x PATH $PYENV_ROOT/bin $PATH
	source (pyenv init -|psub) > /dev/null 2>&1
end

# Go
if test -d $HOME/go
	set GOPATH $HOME/go
	set PATH $PATH $GOPATH/bin
end

# fzf
if test -d $HOME/.fzf/shell
	source $HOME/.fzf/shell/key-bindings.fish
	set -x FZF_DEFAULT_COMMAND 'rg --files --no-ignore --hidden --follow --glob "!.git/*"'
end

# Source command abbreviations
source $HOME/.config/fish/abbreviations.fish > /dev/null 2>&1

source ~/miniconda3/etc/fish/conf.d/conda.fish

direnv hook fish | source
