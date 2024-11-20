# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

. "$HOME/.cargo/env"

. "$HOME/.atuin/bin/env"
||||||| Stash base
# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

echo $(date) .profile SHELL=$SHELL >> "$HOME/.profile_log"

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/.pyenv" ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
fi

if [ -d "$HOME/go" ]; then
    export GOPATH="$HOME/go"
    export PATH="$GOPATH/bin:$PATH"
fi

if [ -d "/usr/local/go/bin" ]; then
    export PATH="/usr/local/go/bin:$PATH"
fi

if [ -d "$HOME/.emacs.d/bin" ]; then
    export PATH="$HOME/.emacs.d/bin:$PATH"
fi

if [ -d "$HOME/.cargo/bin" ]; then
    fi

if [ -d "$HOME/.krew/bin" ]; then
    export PATH="$HOME/.krew/bin:$PATH"
fi


#########################################
# Non Bash Specific ENV Vars

export LANG=en_AU.UTF-8
export LANGUAGE=en_AU.UTF-8


export EDITOR=vim
export VISUAL=vim

# cache pip-installed packages to avoid re-downloading
export PIP_DOWNLOAD_CACHE=$HOME/.pip/cache


export PYTEST_ADDOPTS='--pdbcls=IPython.terminal.debugger:TerminalPdb'

export PATH="$HOME/bin:$HOME/.local/bin:$PATH:/usr/local/sbin"
export LD_LIBRARY_PATH=$HOME/lib:$LD_LIBRARY_PATH
export LIBRARY_PATH=$HOME/lib:$LIBRARY_PATH
export CPATH=$HOME/include
export PATH="$HOME/.poetry/bin:$PATH"


# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad
test -f ~/.pythonrc.py && export PYTHONSTARTUP=~/.pythonrc.py

if type module 2&> /dev/null; then
    module use /g/data/v10/public/modules/modulefiles
    module use /g/data/v10/private/modules/modulefiles
fi

# TODO Fix, don't want to execute recursively by accident
if [[ $HOSTNAME =~ ^gadi-login.* ]]; then
    exec ~/bin/fish
fi

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer


# Added by Toolbox App
export PATH="$PATH:/home/omad/.local/share/JetBrains/Toolbox/scripts"
. "$HOME/.rye/env"
. "$HOME/.cargo/env"
