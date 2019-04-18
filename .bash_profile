# ~/.bash_profile: executed by the command interpreter for login shells.

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "${HOME}/.bashrc" ] ; then
      source "${HOME}/.bashrc"
    fi
fi



export PATH="$HOME/.cargo/bin:$PATH"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/omad/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/omad/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/omad/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/omad/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

