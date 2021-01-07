# fluxctx
complete -f -c fluxctx -a "- 
  --get-alias \
  -f --force \
  -d --delete \
  -h --help \
  (kubectl -n flux get deployments -l app=flux -o=jsonpath='{.items[*].metadata.labels.release}')"
