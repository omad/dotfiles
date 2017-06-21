export PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[33;1m\]\w\[\033[m\]\$ "
export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad

if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

alias ls='ls -GFh'

export PATH="$PATH:$HOME/android-sdk/platform-tools"

export PATH=$PATH:/Applications/Postgres.app/Contents/Versions/latest/bin

export PATH=$PATH:$HOME/bin

# added by Miniconda3 4.1.11 installer
export PATH="$HOME/miniconda3/bin:$PATH"

export PATH="$PATH:/usr/local/sbin"
