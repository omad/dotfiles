function op-to-aws-vault

    set -l op_item (op get item $argv[1] --fields username,password)

    set -lx AWS_ACCESS_KEY_ID (echo $op_item | jq --raw-output .username)
    set -lx AWS_SECRET_ACCESS_KEY (echo $op_item | jq --raw-output .password)
    set -lx AWS_DEFAULT_REGION ap-southeast-2

    aws-vault add --env $argv[1]

end