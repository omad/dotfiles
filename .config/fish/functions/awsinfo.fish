function awsinfo -d "Display current AWS user and context"

    aws sts get-caller-identity
    aws iam list-account-aliases
end

