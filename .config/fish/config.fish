
# Turn off welcome message
set fish_greeting

# Prefer setting up cross shell environment from ~/.profile
# There's still plenty to move across
if status --is-login; and type -q fenv
    fenv source $HOME/.profile
end

# Source command abbreviations
source $HOME/.config/fish/abbreviations.fish > /dev/null 2>&1
source $HOME/.config/fish/aliases.fish > /dev/null 2>&1

# Linuxbrew. Needs to be early so that we can detect available commands.
#if test -d /home/linuxbrew/.linuxbrew
#    /home/linuxbrew/.linuxbrew/bin/brew shellenv | source
#    set -a fish_complete_path /home/linuxbrew/.linuxbrew/share/fish/vendor_completions.d
#end

test -d $HOME/go/bin; and set -a PATH $HOME/go/bin


type -q gh; and gh completion --shell fish | source

if test -d ~/miniconda3/
    status is-interactive && eval ~/miniconda3/bin/conda "shell.fish" "hook" $argv | source
end

type -q direnv; and eval (direnv hook fish)

set -gx MANPAGER 'less -X'
set -x EDITOR vim

if test -f ~/.asdf/asdf.fish
    source ~/.asdf/asdf.fish
end

set -gx AWS_SESSION_TOKEN_TTL 4h

# Colorize man
set MANROFFOPT '-c'
set LESS_TERMCAP_mb (tput bold; tput setaf 2)
set LESS_TERMCAP_md (tput bold; tput setaf 6)
set LESS_TERMCAP_me (tput sgr0)
set LESS_TERMCAP_so (tput bold; tput setaf 3; tput setab 4)
set LESS_TERMCAP_se (tput rmso; tput sgr0)
set LESS_TERMCAP_us (tput smul; tput bold; tput setaf 7)
set LESS_TERMCAP_ue (tput rmul; tput sgr0)
set LESS_TERMCAP_mr (tput rev)
set LESS_TERMCAP_mh (tput dim)

if type -q ssh-pageant
    ssh-pageant -r -a "/tmp/.ssh-pageant-$USERNAME" -S fish | source
    set PATH $PATH /c/msys64/mingw64/bin
    set GIT_GUI_LIB_DIR /c/msys64/usr/share/git-gui/lib
end

# pyenv
if test -d $HOME/.pyenv
    set -x PATH "/home/omad/.pyenv/bin" $PATH
    status --is-interactive; and . (pyenv init -|psub)
    status --is-interactive; and . (pyenv virtualenv-init -|psub)
end

# fzf
if test -d $HOME/.fzf/shell
	source $HOME/.fzf/shell/key-bindings.fish
	set -x FZF_DEFAULT_COMMAND 'rg --files --no-ignore --hidden --follow --glob "!.git/*"'
end
if type -q register-python-argcomplete; and type -q pipx
    register-python-argcomplete --shell fish pipx | source
end

# Fix slow command autocompletion on OS X Catalina
# See: https://github.com/fish-shell/fish-shell/issues/6270
if test (uname) = Darwin
    set -l darwin_version (uname -r | string split .)
    # macOS 15 is Darwin 19
    if test "$darwin_version[1]" = 19 -a "$darwin_version[2]" -le 3
        function __fish_describe_command; end
        exit
    end
end

# fnm
if test -d $HOME/.fnm
    set -a PATH $HOME/.fnm
    fnm env --multi | source
end

if test -d /Applications/Postgres.app/Contents/Versions/latest/bin
    set -a PATH /Applications/Postgres.app/Contents/Versions/latest/bin
end


# Automatically bootstrap fisher
if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

if test -d /opt/TurboVNC/
    set --path -x -a MANPATH :/opt/TurboVNC/man/
    set -a PATH /opt/TurboVNC/bin
end

# Theme and visuals
# See https://github.com/oh-my-fish/theme-bobthefish#configuration
set -g theme_display_k8s_context yes
set -g theme_display_k8s_namespace yes

set -g theme_date_timezone "Australia/Canberra"
set -g theme_title_display_user yes
set -g theme_color_scheme solarized-dark
if string match -q -r 'putty.*' $TERM; or set -q BAD_WINDOWS_FONTS
    set -x BAD_WINDOWS_FONTS yes
    set -g theme_powerline_fonts no
    set -g theme_nerd_fonts no
end

# Back in a proper terminal
if string match -q -r 'rxvt.*' $TERM
    set -e BAD_WINDOWS_FONTS
    set -g theme_powerline_fonts yes
    set -g theme_nerd_fonts yes
end

if test -e "$HOME/.nix-profile/etc/profile.d/nix.sh"; and type -q fenv
  fenv source "$HOME/.nix-profile/etc/profile.d/nix.sh"
end
if test -e "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"; and type -q fenv
    fenv source "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
end
for f in $HOME/.nix-profile/share/fish/vendor_completions.d/*
    source $f
end

test -d ~/.emacs.d/bin; and set -a PATH ~/.emacs.d/bin
test -d ~/.local/bin; and set -a PATH ~/.local/bin
set -a PATH /usr/local/sbin

test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish
set -gx MANPAGER 'less -X'
set -x EDITOR vim


# tabtab source for packages
# uninstall by removing these lines
[ -f ~/.config/tabtab/__tabtab.fish ]; and . ~/.config/tabtab/__tabtab.fish; or true
