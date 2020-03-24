# Defined in /tmp/fish.LdWxxv/cat.fish @ line 1
function cat
	set -l exts md markdown txt

	if not test -f $argv
		echo "File not found: $argv"
		return 0
	end

	if contains (get_ext $argv) $exts; and type -q mdless
		mdless $argv
	else
        if type -q bat
            command bat --style plain --theme OneHalfDark $argv
        else
            command cat $argv
        end
	end
end
