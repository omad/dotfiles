# --- Oh My Zsh specific ------------------------------------------------------
#
# I'm not using oh-my-zsh for too much. Mostly, it makes adding completion
# plugins and prompts easier, but it could be removed if you don't want that
# many completion plugins.
#
# It messes around with and adds things like aliases and other things, so it's
# not for everyone, but it generally works just fine.

# For pycharm
 export PATH=$HOME/bin:/usr/local/bin:$PATH

# Configure zsh-syntax-highlighting (it's an oh-my-zsh plugin)
# (Uses the defaults plus 'brackets', which tell if parens, etc. are unmatched)
ZSH_HIGHLIGHT_HIGHLIGHTERS+=(main brackets)

# Disable Oh My Zsh's annoying auto updates
DISABLE_AUTO_UPDATE="true"

# I prefer not to have autocompletion be case-insensitive, which is the default
export CASE_SENSITIVE="true"

# One of the many oh-my-zsh themes shipped by default
ZSH_THEME=avit
#ZSH_THEME=mh
ZSH_THEME=agnoster
#ZSH_THEME="powerlevel9k/powerlevel9k"
#

# For Conda completion
fpath+="$HOME/github/conda-zsh-completion"

export ZSH="$HOME/.oh-my-zsh/"
plugins=(
    brew
    colored-man-pages
    colorize
    command-time
    common-aliases
    docker
    docker-compose
    git
    git-extras
    github
    golang
    httpie
    man
    npm
    node
    notify
    osx
    pip
    postgres
    pylint
    python
    tmux
    zsh-syntax-highlighting
    zsh-aws-vault
    zsh-autosuggestions
    )

# This allows running `shell` properly within Emacs
if [ -n "$INSIDE_EMACS" ]; then
    export TERM=eterm-color
    plugins=(git)
    ZSH_THEME="simple"
    # for tramp to not hang, need the following. cf:
    # http://www.emacswiki.org/emacs/TrampMode
    #    unsetopt zle
    #    unsetopt prompt_cr
    #    unsetopt prompt_subst
    #    unfunction precmd
    #    unfunction preexec
    #    unset zle_bracketed_paste
    #fi

    #if [[ $TERM == "dumb" ]]; then	# in emacs. Also uses `eterm-color`
    #    PS1='%(?..[%?])%!:%~%# '
fi

source $ZSH/oh-my-zsh.sh

# Change zsh-syntax-highlighting comment color once the defaults have been set
# (Gray in Solarized color palette)

if [ "$ZSH_THEME" != 'simple' ]; then
    ZSH_HIGHLIGHT_STYLES[comment]='fg=green,bold'
fi

# -----------------------------------------------------------------------------

# If command execution time above min. time, plugins will not output time.
ZSH_COMMAND_TIME_MIN_SECONDS=3

# Message to display (set to "" for disable).
ZSH_COMMAND_TIME_MSG="Execution time: %s sec"

# Message color.
ZSH_COMMAND_TIME_COLOR="cyan"

# General zshzle options
setopt autocd                     # cd by just typing in a directory name
setopt completealiases            # tab completion includes aliases
setopt nomatch                    # warn me if a glob doesn't match anything
setopt no_case_glob               # globbing is case insensitive
setopt interactive_comments       # commands preceded with '#' aren't run
setopt menu_complete              # Show completions like Vim (cycle through)
export MENU_COMPLETE=1
#setopt extendedglob              # use #, ^, and ~ as glob characters.
                                  # I've disabled this because it makes zsh
                                  # behave more like bash, at the price of
                                  # giving up features I didn't really use.
                                  # (Uncommented, you have to put quotes
                                  # around these characters to use them)


# Don't try to strip the space between the end of a line and a | character
# (See http://superuser.com/questions/613685/)
ZLE_REMOVE_SUFFIX_CHARS=$' \t\n;&'


# Make ^H and backspace behave correctly
bindkey "^H" backward-delete-char


# Turn on vi keybindings <3 <3 <3 :D and other things
#bindkey -v
bindkey "^?" backward-delete-char
bindkey "^W" backward-kill-word
bindkey "^U" backward-kill-line
bindkey '^R' history-incremental-search-backward

# Sometimes pressing ESC + / quickly (i.e., to do a reverse-i-search with
# bindkey -v turned on) would not work properly. This fixes it.
#vi-search-fix() {
#  zle vi-cmd-mode
#  zle .vi-history-search-backward
#}
#autoload vi-search-fix
#zle -N vi-search-fix
#bindkey -M viins '\e/' vi-search-fix
#
## history search backwords with j/k in vi normal mode
#bindkey -M vicmd 'k' history-beginning-search-backward
#bindkey -M vicmd 'j' history-beginning-search-forward
#

# Initialize zsh history files
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000


zstyle :compinstall filename $HOME/.zshrc

# I really like Vim's "menu completion" style of tab completion where it lists
# all your options
zstyle ':completion:*:*:*:*:*' menu ''
# This is an attempt to combat oh-my-zsh's poor attempt at fuzzy
# spell-correcting tab completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={a-zA-Z}'
# Tab completed files will have the same colors as used for `ls --color`
zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"
# zsh has this really cool feature where you can attach descriptions to any tab
# completion entry. I personally don't like them.

#zstyle ':completion:*' verbose false

# Turn on all the completion stuff
autoload -Uz compinit && compinit
autoload -Uz promptinit && promptinit

# Use zsh's awesome pattern move:
# zmv '*.c' '*.cpp' and it Just Works™ :o
autoload -Uz zmv

# Enable bash compatibility for completion
# For use with python argcomplete
autoload -U bashcompinit
bashcompinit


# --- Homebrew specific -------------------------------------------------------

# I've installed zsh installed through Homebrew, so I add these
# lines to make sure tab completion is properly configured
if [ -e $(which brew &> /dev/null && brew --prefix)/etc/zsh_completion ]; then
  source $(brew --prefix)/etc/zsh_completion
fi

# man pages don't exist for zsh builtins (things like setopt, fg, bg, jobs,
# etc.). Including this snippet lets us run things like 'help setopt' to get
# help.
unalias run-help 2> /dev/null
autoload run-help
# (again, this path is specific to zsh installed through Homebrew)
HELPDIR=/usr/local/share/zsh/help
alias help="run-help"

# -----------------------------------------------------------------------------

export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad



WIFI_SSID=`/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -I | awk '/ SSID/ {print substr($0, index($0, $2))}'`

# If I'm at work
if [ "$WIFI_SSID" = 'GA Staff' ]; then
    # curl -s $(scutil --proxy | grep ProxyAutoConfigURLString | cut -f 2,3 -d :) | grep PROXY | cut -d \" -f 2 | cut -d ' ' -f 2
    export http_proxy=http://proxy.inno.lan:3128
    export no_proxy="localhost,127.0.0.1,localaddress,.localdomain.com"
fi

update_vdi_host () {
    sed -E -i .bak "s/vdi-n[0-9]+.nci.org.au/$1/" ~/.ssh/config
    grep vdi- ~/.ssh/config
}

# added by travis gem
[ -f /Users/omad/.travis/travis.sh ] && source /Users/omad/.travis/travis.sh


function do-vdi {
    ~/miniconda3/envs/py36/bin/vdi launch
    update_vdi_host `~/miniconda3/envs/py36/bin/vdi host`
    ssh vdi
}


#################
# Setup FZF

# Auto-completion
# ---------------
#[[ $- == *i* ]] && source "/usr/local/opt/fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
#source "/usr/local/opt/fzf/shell/key-bindings.zsh"



# Make Plantuml behave itself on OSX
export JAVA_TOOL_OPTIONS=-Djava.awt.headless=true

source /Users/omad/miniconda3/etc/profile.d/conda.sh
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh


# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f /Users/omad/PycharmProjects/dea-orchestration/lambda_functions/execute_ingest/node_modules/tabtab/.completions/serverless.zsh ]] && . /Users/omad/PycharmProjects/dea-orchestration/lambda_functions/execute_ingest/node_modules/tabtab/.completions/serverless.zsh
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[[ -f /Users/omad/PycharmProjects/dea-orchestration/lambda_functions/execute_ingest/node_modules/tabtab/.completions/sls.zsh ]] && . /Users/omad/PycharmProjects/dea-orchestration/lambda_functions/execute_ingest/node_modules/tabtab/.completions/sls.zsh

function magit {
    emacsclient -n -e \(magit-status\)
}

function ap {
    aws-vault --debug exec --keychain=login --no-session --assume-role-ttl=4h $1
}

# peek() { tmux split-window -p 33 "$EDITOR" "$@" || exit; }
peek() { tmux split-window -p 33 /usr/bin/vim "$@"  }

eval "$(direnv hook zsh)"
eval $(thefuck --alias)


pycd () {
    pushd `python -c "import os.path, $1; print(os.path.dirname($1.__file__))"`;
}

# branches that were touched recently
nb() {
    git for-each-ref --sort=-committerdate refs/heads/ -- format='%(committerdate:short) %(authorname) %(refname:short)' | head -n 10
}

# squash!
squash() {
    if [ -n "$1" ]; then
        git reset --soft HEAD~$1 && git commit --edit -m"$(git log --format=%B --reverse HEAD..HEAD@{1})"
    fi
}

# check who uses port
port() {
    lsof -i tcp:"$@"
}

# rebases local branch from origin /master and force pushes it
repush(){
    git fetch origin master
    git stash
    git rebase origin/master
    git push --force
    git stash pop
}

# open pr page on github for current branch
pr () {
    local repo=`git remote -v | grep -m 1 "(push)" | sed -e "s/.*github.com[:/]\(.*\)\.git.*/\1/"`
    local branch=`git name-rev --name-only HEAD`
    echo "... creating pull request for branch \"$branch\" in \"$repo\""
    open "https://github.com/$repo/pull/new/$branch?expand=1"
}

. ~/.aliases

# tabtab source for slss package
# uninstall by removing these lines or running `tabtab uninstall slss`
[[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/slss.zsh ]] && . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/slss.zsh
