
# Setup the Ctrl-T fuzzy file finding shortcut
if type -q fzf
    ~/.nix-profile/bin/fzf --fish | FZF_CTRL_R_COMMAND= FZF_ALT_C_COMMAND= source
end
