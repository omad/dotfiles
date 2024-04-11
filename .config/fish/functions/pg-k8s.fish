function pg-k8s --description "Setup connection to RDS instance"

    kubectl get secrets -A | awk '/db/ {print $1 " " $2}' \
        | SHELL=/usr/bin/bash fzf -n1 --header="Choose k8s secret for credentials" \
            --preview 'kubectl get secrets -n {1} {2} -o yaml | ksd' \
        | read --delimiter ' ' secretnamespace secretname
    or return
    set -l secretdata (kubectl get secret -n $secretnamespace $secretname -o json | ksd)

    #    echo $secretdata

    set -gx PGHOST localhost
    set -gx PGDATABASE (echo $secretdata | jq -r '.stringData["database-name"]')
    set -gx PGUSER (echo $secretdata | jq -r '.stringData["postgres-username"]')
    set -gx PGPASSWORD (echo $secretdata | jq -r '.stringData["postgres-password"]')

    # Default PGDATABASE
    test -z PGDATABASE; or set -x PGDATABASE odc


    echo connecting to $PGUSER

    set local_port 5444

    set -gx DATACUBE_DB_URL postgresql://$PGUSER:$PGPASSWORD@$PGHOST:$local_port/$PGDATABASE

    AWS_PROFILE= npx basti connect --rds-cluster db-aurora-dea-sandbox-eks --local-port $local_port &
    npx wait-on --timeout 120000 --interval 1000 tcp:127.0.0.1:$local_port

    #    kubectl port-forward -n service deployment/pg-proxy :5432 > /tmp/pgforward &
    echo Port forward Up

    set -gx PGPORT $local_port

    set -g BASTI_PID (jobs -lp)

    echo 
    echo Host: localhost
    echo Port: $PGPORT
    echo Database: $PGDATABASE
    echo User: $PGUSER
    echo Password: $PGPASSWORD
    echo
    echo Run pg-k8s-down to shut down the link.


    function pg-k8s-down
        kill $BASTI_PID
        functions -e pg-k8s-down
        set -e PGHOST PGDATABASE PGUSER PGPASSWORD PGPORT PGHOST
    end

end
