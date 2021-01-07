
# Turn off welcome message
set fish_greeting

# Prefer setting up cross shell environment from ~/.profile
# There's still plenty to move across
if status --is-login; and type -q fenv
    fenv source $HOME/.profile
end

# Source command abbreviations
source $HOME/.config/fish/abbreviations.fish > /dev/null 2>&1
source $HOME/.config/fish/aliases.fish > /dev/null 2>&1



if test -d ~/miniconda3/
    eval /home/omad/miniconda3/bin/conda "shell.fish" "hook" $argv | source
end

if type -q direnv
    eval (direnv hook fish)
end

# pyenv
if type -q pyenv
	source (pyenv init -|psub) > /dev/null 2>&1
end

# fzf
if test -d $HOME/.fzf/shell
	source $HOME/.fzf/shell/key-bindings.fish
	set -x FZF_DEFAULT_COMMAND 'rg --files --no-ignore --hidden --follow --glob "!.git/*"'
end
if type -q register-python-argcomplete; and type -q pipx
    register-python-argcomplete --shell fish pipx | source
end


test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish
set -gx MANPAGER 'less -X'
set -x EDITOR vim
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

# Linuxbrew
if test -d /home/linuxbrew/.linuxbrew
    /home/linuxbrew/.linuxbrew/bin/brew shellenv | source
    set -a fish_complete_path /home/linuxbrew/.linuxbrew/share/fish/vendor_completions.d
end
