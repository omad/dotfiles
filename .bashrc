#
#  Default initial .bashrc for the ac.  
#
#  Much of your environment should be setup in your .profile or
#  .bash_profile file.  The environment configured there will be
#  inherited by subshells, scripts, batch job etc.  This file 
#  (.bashrc) should only be used to define aliases, functions and, 
#  where necessary, terminal settings.  By default this file will 
#  be sourced for every bash shell and script so avoid unnecessary 
#  actions.  
#
#  See your .profile file for examples of how to add paths to 
#  software in your environment.
#  See http://nf.nci.org.au/facilities/software/modules.php
#  for instructions on how to set your environment to use specific
#  software packages.
#
#  $Id: default.bashrc,v 1.3 2008/12/18 00:08:09 dbs900 Exp $
#

#  Avoid going through here more than once in a shell

#ps -o args= $PPID
#caller
#echo "FUNCNAME: $FUNCNAME BASH_SOURCE: $BASH_SOURCE BASH_LINENO: $BASH_LINENO BASH_SUBSHELL: $BASH_SUBSHELL "
#echo $@
#echo $*


[ -n "$nf_bashrc_sourced" ] && return


#  Source global definitions to get the module command defined.
#  If you remove this from your file and/or you reset the BASH_ENV (or
#  ENV) variables,  you risk getting "module command not found"
#  errors from batch jobs.

if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi



nf_bashrc_sourced=YES


# module load dot adds the current directory to the end of your commands
# search path
module load dot

#  Find the Fortran and C compilers.
#  #  Use module swap to change the compilers you wish to use.
#  #  Compilers do not need to be loaded to run executables,
#  #  only to build them.
#

#module load intel-fc
#module load intel-cc

#
#
#  # module load openmpi will give you the default MPI on the system.
#  # Some packages may use other MPI libraries which will conflict
#  # with this.  Use module swap to change the MPI library used.
#
#module load openmpi
#

HISTCONTROL=ignoredups:erasedups  # no duplicate entries
HISTSIZE=100000                   # big big history
HISTFILESIZE=100000               # big big history
shopt -s histappend               # append to history, don't overwrite it

# Save and reload the history after each command finishes
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"


export LANG=en_AU.UTF-8
export LC_ALL=en_AU.UTF-8

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
