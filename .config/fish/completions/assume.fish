
# Nix/Home-manager is something is installed some snafu'd completions, this removes them all
set -l foo (complete -c assume)
eval complete -c (string split ' ' $foo)[2..] -e


complete --command assume --no-files 
complete -c assume --arguments "(aws configure list-profiles)"
complete -c assume -s h -l help
complete -c assume -s c --long console
complete -c assume -s s --long service --arguments 's3 cognito dynamodb rds iam cloudfront ec2 eks' --require-parameter
