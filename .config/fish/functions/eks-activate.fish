function eks-activate
    aws eks update-kubeconfig --name $argv[1]
end

