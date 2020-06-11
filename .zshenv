

export PATH=$PATH:/Applications/Postgres.app/Contents/Versions/latest/bin
export PATH=$PATH:/usr/local/lib/ruby/gems/2.6.0/bin

export PATH="$PATH:/usr/local/sbin"
export PATH="/Users/omad/bin:/Users/omad/.local/bin:$PATH"

export PATH="$HOME/Library/Python/3.7/bin:$PATH"


# Setup fzf
# ---------
if [[ ! "$PATH" == */usr/local/opt/fzf/bin* ]]; then
    export PATH="$PATH:/usr/local/opt/fzf/bin"
fi
if [ -e /home/omad/.nix-profile/etc/profile.d/nix.sh ]; then . /home/omad/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
