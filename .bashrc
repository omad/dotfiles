# ~/.bashrc: executed by bash(1) for non-login shells.

#  Source global definitions to get the module command defined.
#  If you remove this from your file and/or you reset the BASH_ENV (or
#  ENV) variables,  you risk getting "module command not found"
#  errors from batch jobs.

if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi
if [ -f /etc/bash.bashrc ]; then
    . /etc/bash.bashrc
fi

#source ~/.profile

system_type=$(uname -s)
if [ "$system_type" = "Darwin" ]; then
    export PATH="$PATH:$HOME/android-sdk/platform-tools"
    export PATH=$PATH:/Applications/Postgres.app/Contents/Versions/latest/bin

    if [ -f $(brew --prefix)/etc/bash_completion ]; then
      . $(brew --prefix)/etc/bash_completion
    fi
    WIFI_SSID=`/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -I | awk '/ SSID/ {print substr($0, index($0, $2))}'`

    # If I'm at work
    if [ "$WIFI_SSID" = 'GA Staff' ]; then
        export http_proxy=proxy.inno.lan:3128
        export no_proxy="localhost,127.0.0.1,localaddress,.localdomain.com"
    fi

    # scutil --proxy
    # ...
    #  ProxyAutoConfigURLString : http://win-dhcp-prod03.inno.lan/wpad.dat
    # curl http://win-dhcp-prod03.inno.lan/wpad.dat
    #
    #  return "PROXY proxy.inno.lan:3128";
    #

fi

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

#  Avoid going through here more than once in a shell

[ -n "$nf_bashrc_sourced" ] && return
nf_bashrc_sourced=YES

system_type=$(uname -s)

[[ $TERM == "dumb" ]] && PS1="$ " && return

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

if command_exists direnv; then
    eval "$(direnv hook bash)"
fi

if [[ $system_type =~ MINGW || $system_type =~ MSYS ]]; then
    # ssh-pageant
    # Not able to use file sockets in windows...
    #eval $(ssh-pageant.exe -r -a "/c/Temp/.ssh-pageant-$USERNAME")
    # eval $(ssh-pageant)
    if command_exists ssh-pageant; then
        eval $(ssh-pageant -r -a "/tmp/.ssh-pageant-$USERNAME")
    fi

    export PATH=$PATH:/c/msys64/mingw64/bin
    export GIT_GUI_LIB_DIR=/c/msys64/usr/share/git-gui/lib
fi

# For install ruby gems into user home
if command_exists ruby; then
    gems_path=$(ruby -rubygems -e "puts Gem.user_dir" 2> /dev/null)
    export PATH=$PATH:${gems_path:+:${gems_path}/bin}
fi
 

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

# added by travis gem
[ -f $HOME/.travis/travis.sh ] && source $HOME/.travis/travis.sh



if [[ `hostname` =~ vdi ]]; then
    mkdir -p /local/u46/dra547/tmp/dotcondapkgcache
    mkdir -p /local/u46/dra547/tmp/dotcache
    export PATH="${PATH}:${HOME}/src/damootils/scripts"
    export PIP_DOWNLOAD_CACHE=$TMPDIR/pipcache
    mkdir -p $PIP_DOWNLOAD_CACHE
    export CARGO_HOME=/local/u46/dra547/cargo
    export RUSTUP_HOME=/local/u46/dra547/rustup
    export PATH=$PATH:$CARGO_HOME/bin
fi
if [[ ! -S ~/.ssh/ssh_auth_sock && -S "$SSH_AUTH_SOCK" ]]; then
    ln -sf $SSH_AUTH_SOCK ~/.ssh/ssh_auth_sock
fi


###########################################
# Setup Prompt

. ~/.bash/git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
export PS1='\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[33;1m\]\w\[\033[m\] $(__git_ps1 "(%s) ")\$ '



# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

#########################################
# Add a bunch of programmable completions

export BASH_COMPLETION_USER_DIR=$HOME/share
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
if [ -f $HOME/.bash/fd.bash_completion ]; then
  . $HOME/.bash/fd.bash_completion
fi
[[ $PS1 && -f $HOME/share/bash-completion/bash_completion ]] && \
        . $HOME/share/bash-completion/bash_completion

 [[ $PS1 && -f ~/share/bash-completion/bash_completion ]] && \
         . ~/share/bash-completion/bash_completion

[[ $PS1 && -f $HOME/share/tmux.completion.bash ]] && \
    source $HOME/share/tmux.completion.bash


. ~/.bash/git-completion.bash
complete -C aws_completer aws

if command_exists kubectl; then
    source <(kubectl completion bash)
fi



function lb() {
    # thanks https://routley.io/tech/2017/11/23/logbook.html
    mkdir -p ~/logbook
    vim ~/logbook/$(date '+%Y-%m-%d').md
}
alias qsub_interactive="qsub -I -q express -l wd,walltime=5:00:00,mem=20GB,ncpus=1 -P u46"

[ -f ~/.fzf.bash ] && source ~/.fzf.bash



############################
# ODC Administration Aliases

alias datacube_activity="psql -h 130.56.244.105 datacube -c 'select datname, usename, state, application_name, current_timestamp-query_start as duration from pg_stat_activity';"
alias datacube_sizes="psql -h 130.56.244.105 datacube -c \"SELECT nspname || '.' || relname AS relation, pg_size_pretty(pg_relation_size(C.oid)) AS size FROM pg_class C LEFT JOIN pg_namespace N ON (N.oid = C.relnamespace) WHERE nspname NOT IN ('pg_catalog', 'information_schema') ORDER BY pg_relation_size(C.oid) DESC;\""
alias datacube_pgbouncer_activity="psql -h 130.56.244.105 -p 6432 pgbouncer -U cube_admin -c 'show clients;'"
alias datacube_pgbouncer_pools="psql -h 130.56.244.105 -p 6432 pgbouncer -U cube_admin -c 'show pools;'"

dc-dump-ds () {
   local uuid=${1}
   local dbname=${2:-"datacube"}
   local host=${3:-"130.56.244.105"}
   local port=${4:-6432}
   if [[ -z $uuid ]]; then
       echo "Usage: dc-dump-ds UUID [DBNAME:$dbname] [HOST:$host] [PORT:$port]"
       return
    fi

   cat <<EOF | psql --no-psqlrc --quiet -t -h "${host}" -p "${port}" "${dbname}"
        select metadata
        from agdc.dataset
        where id = '${uuid}';
EOF
}

dc-dump-product () {
   local name=${1}
   local dbname=${2:-"datacube"}
   local host=${3:-"130.56.244.105"}
   local port=${4:-6432}

   cat <<EOF | psql --no-psqlrc --quiet -t -h "${host}" -p "${port}" "${dbname}"
        select definition
        from agdc.dataset_type
        where name = '${name}';
EOF
}

dc-index-eo3 () {
    fd odc-metadata.yaml $1 | tar cvf - --files-from=-  | dc-index-from-tar -E dra547 --eo3 --ignore-lineage --protocol file -
}

# Fast Node version Manager (FNM)
if command_exists fnm; then
    export PATH=/home/547/dra547/.fnm:$PATH
    eval "`fnm env --multi`"
fi

# added by travis gem
[ -f /home/omad/.travis/travis.sh ] && source /home/omad/.travis/travis.sh

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/omad/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/omad/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/omad/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/omad/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup

if [ -f "/home/omad/miniconda3/etc/profile.d/mamba.sh" ]; then
    . "/home/omad/miniconda3/etc/profile.d/mamba.sh"
fi
# <<< conda initialize <<<


complete -C /nix/store/l82nqzrn2s442d40zpa1r2jg7a2chrlz-s5cmd-1.4.0/bin/s5cmd s5cmd
