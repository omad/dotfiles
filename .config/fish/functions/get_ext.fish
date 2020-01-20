# Defined in /tmp/fish.WByI8Q/get_ext.fish @ line 1
function get_ext --description 'Get the file extension from the argument'
	set -l splits (string split "." $argv)
	echo $splits[-1]
end
