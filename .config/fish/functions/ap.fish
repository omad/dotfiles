# Defined in /tmp/fish.iWaLOE/ap.fish @ line 2
function ap --description 'Start aws-vault shell session'
    #    aws-vault exec $argv[1] --no-session --duration=4h --
    aws-vault exec $argv[1] --duration=4h --
end
