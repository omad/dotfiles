export PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[33;1m\]\w\[\033[m\]\$ "
export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad

system_type=$(uname -s)

if [ "$system_type" = "Darwin" ]; then
    if [ -f $(brew --prefix)/etc/bash_completion ]; then
      . $(brew --prefix)/etc/bash_completion
    fi
    export PATH="$PATH:$HOME/android-sdk/platform-tools"

    export PATH=$PATH:/Applications/Postgres.app/Contents/Versions/latest/bin
    # added by Miniconda3 4.1.11 installer
    export PATH="$HOME/miniconda3/bin:$PATH"
fi

alias ls='ls --color -GFh'


export PATH="$HOME/bin:$HOME/.local/bin:$PATH:/usr/local/sbin"
export LD_LIBRARY_PATH=$HOME/lib:$LD_LIBRARY_PATH
export LIBRARY_PATH=$HOME/lib:$LIBRARY_PATH
export CPATH=$HOME/include

