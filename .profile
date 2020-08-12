# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

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

if [ -d "$HOME/.pyenv" ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
fi

if [ -d "$HOME/go" ]; then
    export GOPATH="$HOME/go"
    export PATH="$GOPATH/bin:$PATH"
fi

if [ -d "$HOME/.emacs.d/bin" ]; then
    export PATH="$HOME/.emacs.d/bin:$PATH"
fi

if [ -d "$HOME/.cargo/bin" ]; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi



#########################################
# Non Bash Specific ENV Vars

export LANG=en_AU.UTF-8
export LANGUAGE=en_AU.UTF-8


export EDITOR=vim
export VISUAL=vim

# cache pip-installed packages to avoid re-downloading
export PIP_DOWNLOAD_CACHE=$HOME/.pip/cache


export PGHOST=agdc-db.nci.org.au
export PGDATABASE=datacube
export PYTEST_ADDOPTS='--pdbcls=IPython.core.debugger:Pdb'

export PATH="$HOME/bin:$HOME/.local/bin:$PATH:/usr/local/sbin"
export LD_LIBRARY_PATH=$HOME/lib:$LD_LIBRARY_PATH
export LIBRARY_PATH=$HOME/lib:$LIBRARY_PATH
export CPATH=$HOME/include
export PATH="$HOME/.poetry/bin:$PATH"


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


# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad
test -f ~/.pythonrc.py && export PYTHONSTARTUP=~/.pythonrc.py

if type module &> /dev/null; then
    module use /g/data/v10/public/modules/modulefiles
    module use /g/data/v10/private/modules/modulefiles
fi

# TODO Fix, don't want to execute recursively by accident
if [[ $(hostname) =~ ^gadi-login.* ]]; then
    exec ~/bin/fish
fi

if [ -e /home/omad/.nix-profile/etc/profile.d/nix.sh ]; then . /home/omad/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
