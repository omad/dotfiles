# Named 'zz.' to run after fzf.fish
# Otherwise the atuin history search binding is overridden
#


fzf_configure_bindings --history= --directory=ctrl-f

source "$HOME/.atuin/bin/env.fish"



atuin init fish | source

