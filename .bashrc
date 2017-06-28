
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
GREP_OPTIONS='--color=auto'
for PATTERN in .cvs .git .hg .svn .work; do
  GREP_OPTIONS="$GREP_OPTIONS --exclude-dir=$PATTERN"
done
export GREP_OPTIONS


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


# pip should only run if there is a virtualenv currently activated
export PIP_REQUIRE_VIRTUALENV=true
# cache pip-installed packages to avoid re-downloading
export PIP_DOWNLOAD_CACHE=$HOME/.pip/cache


system_type=$(uname -s)

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


# added by travis gem
[ -f $HOME/.travis/travis.sh ] && source $HOME/.travis/travis.sh






alias pbcopy='xsel --clipboard --input'
alias pbpaste='xsel --clipboard --output'

alias parallel='parallel --citation'

export PGHOST=agdc-db.nci.org.au
export PGDATABASE=datacube

alias git=hub

alias quota='quota -sQ' # Human readable and ignore NFS errors

export PYTEST_ADDOPTS='--pdbcls=IPython.terminal.debugger:TerminalPdb'


if [[ `hostname` =~ vdi ]]; then
    mkdir -p /local/u46/dra547/tmp/dotcondapkgcache
    mkdir -p /local/u46/dra547/tmp/dotcache
    export PATH=$PATH:$HOME/src/damootils/scripts
fi


complete -C aws_completer aws

