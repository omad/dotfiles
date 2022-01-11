function pg-k8s --argument secretname

    set -l secretdata (kubectl get secret -n processing $secretname -o json | ksd)

    #    echo $secretdata

    set -gx PGHOST localhost
    set -gx PGDATABASE (echo $secretdata | jq -r '.stringData["database-name"]')
    set -gx PGUSER (echo $secretdata | jq -r '.stringData["postgres-username"]')
    set -gx PGPASSWORD (echo $secretdata | jq -r '.stringData["postgres-password"]')

    echo connecting to $PGUSER

    kubectl port-forward -n service deployment/pg-proxy :5432 > /tmp/pgforward &
    sleep 2
    echo Port forward Up
    string match -qr '127.0.0.1:(?<PGPORT>\d+)' < /tmp/pgforward
    set -gx PGPORT $PGPORT

    set -g K8S_PF_PID (jobs -lp)

    echo 
    echo Host: localhost
    echo Port: $PGPORT
    echo Database: $PGDATABASE
    echo User: $PGUSER
    echo Password: $PGPASSWORD
    echo
    echo Run pg-k8s-down to shut down the link.


    function pg-k8s-down
        kill $K8S_PF_PID
        functions -e pg-k8s-down
        set -e PGHOST PGDATABASE PGUSER PGPASSWORD PGPORT PGHOST
    end

end
