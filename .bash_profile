
# source the users bashrc if it exists
if [ -f "${HOME}/.bashrc" ] ; then
  source "${HOME}/.bashrc"
fi


export PS1='\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[33;1m\]\w\[\033[m\] $(__git_ps1 "(%s) ")\$ '

export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad


system_type=$(uname -s)
if [ "$system_type" = "Darwin" ]; then
    if [ -f $(brew --prefix)/etc/bash_completion ]; then
      . $(brew --prefix)/etc/bash_completion
    fi
    export PATH="$PATH:$HOME/android-sdk/platform-tools"

    export PATH=$PATH:/Applications/Postgres.app/Contents/Versions/latest/bin

    export PATH="$HOME/miniconda3/bin:$PATH"
fi
alias ls='ls --color -GFh'


export PATH="$HOME/.local/bin:$PATH:/usr/local/sbin"
export LD_LIBRARY_PATH=$HOME/lib:$LD_LIBRARY_PATH
export LIBRARY_PATH=$HOME/lib:$LIBRARY_PATH
export CPATH=$HOME/include



# Set PATH so it includes user's private bin if it exists
if [ -d "${HOME}/bin" ] ; then
    PATH="${HOME}/bin:${PATH}"
fi

# Set MANPATH so it includes users' private man if it exists
if [ -d "${HOME}/man" ]; then
  MANPATH="${HOME}/man:${MANPATH}"
fi

if [ -d "${HOME}/share/man" ]; then
    MANPATH=$MANPATH:$HOME/share/man
fi

# Set INFOPATH so it includes users' private info if it exists
if [ -d "${HOME}/info" ]; then
  INFOPATH="${HOME}/info:${INFOPATH}"
fi
