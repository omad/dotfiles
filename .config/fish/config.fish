
# Turn off welcome message
set fish_greeting

if status --is-login
    echo (date) config.fish TERM_PROGRAM = $TERM_PROGRAM >> ~/.profile_log
end

set -gx PATH $HOME/bin $PATH
set -gx PATH $HOME/.cargo/bin $PATH

#if test "$TERM_PROGRAM" = "vscode"
  # ~/.profile is run by the login shell (this is what ssh uses)
  # ~/.bashrc is run by the interactive shell (this is what vscode uses)
  # Therefore, we only need to change the shell to zsh here since
  # vscode will run ~/.bashrc for us.
  #  exec zsh -l
  #end

# Prefer setting up cross shell environment from ~/.profile
# There's still plenty to move across
if status --is-login; and type -q fenv
    fenv source $HOME/.profile
end

set -x DOCKER_BUILDKIT 1

if status is-interactive
    # Fancy prompt history using atuin
    type -q atuin; and atuin init fish | source

    # Fancy prompt using starship
    type -q starship; and starship init fish | source
end


# Source command abbreviations
source $HOME/.config/fish/abbreviations.fish > /dev/null 2>&1
source $HOME/.config/fish/autocorrections.fish > /dev/null 2>&1
source $HOME/.config/fish/aliases.fish > /dev/null 2>&1


set -p fish_complete_path $HOME/.nix-profile/share/fish/vendor_completions.d/

test -f completions/granted_completer_fish.fish; and source completions/granted_completer_fish.fish

test -d $HOME/go/bin; and set -a PATH $HOME/go/bin

type -q direnv; and eval (direnv hook fish)

set -x EDITOR vim

set -gx AWS_SESSION_TOKEN_TTL 4h

# Colorize man
#set -gx MANPAGER 'less -X
set -gx MANPAGER 'less -R'
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

if test -d $HOME/.fly
    set -x FLYCTL_INSTALL "$HOME/.fly"
    set -a PATH "$FLYCTL_INSTALL/bin"
end

set -a PATH "$HOME/.pulumi/bin"


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

set -gx PATH $PATH $HOME/.krew/bin


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


# pnpm
set -gx PNPM_HOME "/home/omad/.local/share/pnpm"
if not string match -q -- $PNPM_HOME $PATH
  set -gx PATH "$PNPM_HOME" $PATH
end
# pnpm end

# Initialising pixi integration is slow
#fish_add_path /home/omad/.pixi/bin
#pixi completion --shell fish | source

# >>> mamba initialize >>>
# !! Contents within this block are managed by 'mamba init' !!
set -gx MAMBA_EXE "/home/omad/.local/bin/micromamba"
set -gx MAMBA_ROOT_PREFIX "/home/omad/micromamba"
$MAMBA_EXE shell hook --shell fish --root-prefix $MAMBA_ROOT_PREFIX | source
# <<< mamba initialize <<<
fish_add_path /home/omad/.pixi/bin

# TODO: Maybe should only be is-interactive
set -x GRANTED_ALIAS_CONFIGURED true
alias assume="source ~/bin/assume.fish"

