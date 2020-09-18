
# Turn off welcome message
set fish_greeting

if status --is-login; and type -q fenv
    fenv source $HOME/.profile
end

if type -q conda
   # >>> conda initialize >>>
   # !! Contents within this block are managed by 'conda init' !!
   eval conda "shell.fish" "hook" $argv | source
   # <<< conda initialize <<<
end

if type -q gh
    gh completion --shell fish | source
end

if test -f /c/w10dev/miniconda3/Scripts/conda.exe
    /c/w10dev/miniconda3/Scripts/conda shell.fish hook | source
#    conda activate base
end


if test -f /opt/Modules/v4.3.0/init/fish
    source /opt/Modules/v4.3.0/init/fish
    source /opt/Modules/v4.3.0/init/fish_completion
end

set -gx MANPAGER 'less -X'
set -x EDITOR vim


if type -q nvim
    alias vim nvim
end

# The fd executable was renamed in Ubuntu
if type -q fdfind
    alias fd fdfind
end

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
if type -q pyenv
	source (pyenv init -|psub) > /dev/null 2>&1
end

# fzf
if test -d $HOME/.fzf/shell
	source $HOME/.fzf/shell/key-bindings.fish
	set -x FZF_DEFAULT_COMMAND 'rg --files --no-ignore --hidden --follow --glob "!.git/*"'
end

# Ruby Gems
#if test -d $HOME/.gem/ruby
#    set PATH $PATH $HOME/.gem/ruby
if type -q ruby
    set PATH $PATH (ruby -e "puts Gem.user_dir")/bin
    #    $(ruby -rubygems -e "puts Gem.user_dir")/bin
end

# Source command abbreviations
source $HOME/.config/fish/abbreviations.fish > /dev/null 2>&1

if test -f ~/miniconda3/etc/fish/conf.d/conda.fish
    source ~/miniconda3/etc/fish/conf.d/conda.fish
end

if type -q direnv
    # direnv hook fish | source
    eval (direnv hook fish)
end

# register-python-argcomplete --shell fish pipx | .

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
    set PATH $HOME/.fnm $PATH
    fnm env --multi | source
end

if test -d /Applications/Postgres.app/Contents/Versions/latest/bin
    set PATH $PATH /Applications/Postgres.app/Contents/Versions/latest/bin
end


# Ubuntu related from /etc/profile.d
set --local snap_bin_path /snap/bin
if test -d $snap_bin_path
    set PATH $PATH $snap_bin_path
end
if set -q XDG_DATA_DIRS
    set --path XDG_DATA_DIRS /usr/local/share /usr/share
end
set local snap_xdg_path /var/lib/snapd/desktop
set XDG_DATA_DIRS $snap_xdg_path

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
if test -d "~/.emacs.d/bin"
    set PATH $PATH "~/.emacs.d/bin"
end

