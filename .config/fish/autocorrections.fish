## Set up one or more autocorrections
# using the new and improved abbreviations support in fish
# which allows replacing tokens anywhere, and controlling it with
# a function.
#
# This one replaces 'uninstall' with 'remove' when using the 'apt' command.
# Something I forever get wrong since I'm used to conda.
function _autocorrect
    if contains apt (commandline) 
        echo remove
        return 0
    end
end

abbr --add uninstall --position anywhere --function _autocorrect
