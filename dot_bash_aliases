
# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'


alias pbcopy='xsel --clipboard --input'
alias pbpaste='xsel --clipboard --output'


if command_exists hub; then
    alias git=hub
fi
alias ls='ls --color'


alias quota='quota -sQ' # Human readable and ignore NFS errors

# Grep default options, that you don't have to type with the command
grep_options='--color=auto'
for PATTERN in .git .hg .svn .work; do
  grep_options="$grep_options --exclude-dir=$PATTERN"
done
alias grep="grep $grep_options"

