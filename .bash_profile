
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


if [[ $(hostname) =~ ^gadi-login.* ]]; then
    exec ~/bin/fish
fi
