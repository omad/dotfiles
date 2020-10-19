function do-pg-fwd
    kubectl port-forward deployment/pg-proxy 5432:5432 -n service
end

