

export PATH=$PATH:/Applications/Postgres.app/Contents/Versions/latest/bin

export PATH="$PATH:/usr/local/sbin"
export PATH="/Users/omad/bin:/Users/omad/.local/bin:$PATH"



# Setup fzf
# ---------
if [[ ! "$PATH" == */usr/local/opt/fzf/bin* ]]; then
    export PATH="$PATH:/usr/local/opt/fzf/bin"
fi