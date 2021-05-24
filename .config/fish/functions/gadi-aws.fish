# Defined in /tmp/fish.AAmkmV/gadi-aws.fish @ line 1
function gadi-aws
    ssh -t gadi  'export AWS_SECURITY_TOKEN='"'$AWS_SECURITY_TOKEN'"';export AWS_ACCESS_KEY_ID='"'$AWS_ACCESS_KEY_ID'"';export AWS_SECRET_ACCESS_KEY="'$AWS_SECRET_ACCESS_KEY'"; module use /g/data/v10/public/modules/modulefiles; module load dea; PATH="$HOME/bin:$PATH"; exec /bin/bash' # exec ./bin/fish'
end
