# ~/.bashrc: executed by bash(1) for non-login shells.

system_type=$(uname -s)
if [ "$system_type" = "Darwin" ]; then
    export PATH="$PATH:$HOME/android-sdk/platform-tools"
    export PATH=$PATH:/Applications/Postgres.app/Contents/Versions/latest/bin

fi

export PATH="$HOME/.local/bin:$PATH:/usr/local/sbin"
export LD_LIBRARY_PATH=$HOME/lib:$LD_LIBRARY_PATH
export LIBRARY_PATH=$HOME/lib:$LIBRARY_PATH
export CPATH=$HOME/include



if [ -d "${HOME}/miniconda3/bin" ]; then
    export PATH="$HOME/miniconda3/bin:$PATH"
fi
# set PATH so it includes user's private bin directories
PATH="$HOME/bin:$HOME/.local/bin:$PATH"


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

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

#  Avoid going through here more than once in a shell

[ -n "$nf_bashrc_sourced" ] && return

#  Source global definitions to get the module command defined.
#  If you remove this from your file and/or you reset the BASH_ENV (or
#  ENV) variables,  you risk getting "module command not found"
#  errors from batch jobs.

if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi


nf_bashrc_sourced=YES

system_type=$(uname -s)

HISTCONTROL=ignoredups:erasedups  # no duplicate entries
HISTSIZE=10000                   # big big history
HISTFILESIZE=10000               # big big history
shopt -s histappend               # append to history, don't overwrite it

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Save and reload the history after each command finishes
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

command_exists () {
    type "$1" &> /dev/null ;
}

if command_exists module; then
    module use /g/data/v10/public/modules/modulefiles --append
fi


export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8


EDITOR=vim
VISUAL=vim

# Grep default options, that you don't have to type with the command
grep_options='--color=auto'
for PATTERN in .git .hg .svn .work; do
  grep_options="$grep_options --exclude-dir=$PATTERN"
done
alias grep="grep $grep_options"

if [[ $system_type =~ MINGW ]]; then
    # ssh-pageant
    # Not able to use file sockets in windows...
    #eval $(ssh-pageant.exe -r -a "/c/Temp/.ssh-pageant-$USERNAME")
    eval $(ssh-pageant)

    export PATH=$PATH:/c/msys64/mingw64/bin
    export GIT_GUI_LIB_DIR=/c/msys64/usr/share/git-gui/lib
fi

# For install ruby gems into user home
if command_exists ruby; then
    export PATH=$PATH:$(ruby -rubygems -e "puts Gem.user_dir")/bin
fi

function gimmesomedatacube {
    module load agdc-py3-prod/1.4.1
    unset PYTHONNOUSERSITE
}


if [ -n "$TMUX" ]; then
    function refresh-tmux-env-vars {
        eval $(tmux showenv -s DISPLAY)
        eval $(tmux showenv -s SSH_AUTH_SOCK)
    }
else
    function refresh-tmux-env-vars { true; }
fi


function preexec {
    refresh-tmux-env-vars
}


# cache pip-installed packages to avoid re-downloading
export PIP_DOWNLOAD_CACHE=$HOME/.pip/cache



if [ "$system_type" = "Darwin" ]; then
    WIFI_SSID=`/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -I | awk '/ SSID/ {print substr($0, index($0, $2))}'`

    # If I'm at work
    if [ "$WIFI_SSID" = 'GA Staff' ]; then
        export http_proxy=proxy.inno.lan:3128
    fi

# scutil --proxy
# ...
#  ProxyAutoConfigURLString : http://win-dhcp-prod03.inno.lan/wpad.dat
# curl http://win-dhcp-prod03.inno.lan/wpad.dat
#
#  return "PROXY proxy.inno.lan:3128";
#
fi

export PGHOST=agdc-db.nci.org.au
export PGDATABASE=datacube

# added by travis gem
[ -f $HOME/.travis/travis.sh ] && source $HOME/.travis/travis.sh



alias pbcopy='xsel --clipboard --input'
alias pbpaste='xsel --clipboard --output'

alias parallel='parallel --citation'

export PGHOST=agdc-db.nci.org.au
export PGDATABASE=datacube

if command_exists hub; then
    alias git=hub
fi
alias ls='ls --color'

alias quota='quota -sQ' # Human readable and ignore NFS errors

export PYTEST_ADDOPTS='--pdbcls=IPython.terminal.debugger:TerminalPdb'


if [[ `hostname` =~ vdi ]]; then
    mkdir -p /local/u46/dra547/tmp/dotcondapkgcache
    mkdir -p /local/u46/dra547/tmp/dotcache
    export PATH=$PATH:$HOME/src/damootils/scripts
    export PIP_DOWNLOAD_CACHE=$TMPDIR/pipcache
    mkdir -p $PIP_DOWNLOAD_CACHE
fi


complete -C aws_completer aws

. ~/.bash/git-completion.bash
. ~/.bash/git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1



# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'


# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

if [ -f $HOME/.bash/hub.bash_completion ]; then
  . $HOME/.bash/hub.bash_completion
fi


export PS1='\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[33;1m\]\w\[\033[m\] $(__git_ps1 "(%s) ")\$ '

export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad


system_type=$(uname -s)
if [ "$system_type" = "Darwin" ]; then
    if [ -f $(brew --prefix)/etc/bash_completion ]; then
      . $(brew --prefix)/etc/bash_completion
    fi
fi

