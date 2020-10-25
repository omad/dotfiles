function awslogin -d "Login to AWS using aws-azure-login"

    set -x AZURE_DEFAULT_ROLE_ARN (awk '{print $1}' ~/.aws/roles | fzf --height 6 --prompt="Choose Role: " | read -l result; and awk "/$result/ {print \$2}" ~/.aws/roles)

    set -x AZURE_DEFAULT_USERNAME (op get item pyjawm7tsqvbmgmc2ub3i2ioxa --fields username)
    set -x AZURE_DEFAULT_PASSWORD (op get item pyjawm7tsqvbmgmc2ub3i2ioxa --fields password)

    op get totp pyjawm7tsqvbmgmc2ub3i2ioxa

    aws-azure-login --no-prompt

end

