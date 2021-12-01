
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

set -a fish_complete_path $HOME/.nix-profile/share/fish/vendor_completions.d/
set -a fish_complete_path $HOME/.nix-profile/etc/fish/completions


test -d $HOME/go/bin; and set -a PATH $HOME/go/bin

if test -d ~/miniconda3/
    status is-interactive && eval ~/miniconda3/bin/conda "shell.fish" "hook" $argv | source
end

type -q direnv; and eval (direnv hook fish)

set -x EDITOR vim

set -gx AWS_SESSION_TOKEN_TTL 4h

# Colorize man
set -gx MANPAGER 'less -X'
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

# fzf
if test -d $HOME/.fzf/shell
	source $HOME/.fzf/shell/key-bindings.fish
	set -x FZF_DEFAULT_COMMAND 'rg --files --no-ignore --hidden --follow --glob "!.git/*"'
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

# Fish Node Manager
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

if test -e "$HOME/.nix-profile/etc/profile.d/nix.sh"; and type -q fenv
  fenv source "$HOME/.nix-profile/etc/profile.d/nix.sh"
end
if test -e "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"; and type -q fenv
    fenv source "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
end

test -d ~/.emacs.d/bin; and set -a PATH ~/.emacs.d/bin
test -d ~/.local/bin; and set -a PATH ~/.local/bin
set -a PATH /usr/local/sbin

test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish


# tabtab source for packages
# uninstall by removing these lines
[ -f ~/.config/tabtab/__tabtab.fish ]; and . ~/.config/tabtab/__tabtab.fish; or true

# Add git/fzf keybindings from https://brettterpstra.com/2021/11/25/git-better-with-fzf-and-fish/
if status is-interactive && test -f ~/.config/fish/custom/git_fzf.fish
	source ~/.config/fish/custom/git_fzf.fish
	git_fzf_key_bindings
end

starship init fish | source
