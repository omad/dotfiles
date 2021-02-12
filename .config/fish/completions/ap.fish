
complete -ec ap

complete -c ap -xa '(aws-vault list | tail -n +3 | cut -d" " -f 1)'
