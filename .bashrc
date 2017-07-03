
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
HISTSIZE=100000                   # big big history
HISTFILESIZE=100000               # big big history
shopt -s histappend               # append to history, don't overwrite it

# Save and reload the history after each command finishes
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

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
