{
  pkgs,
  ...
}:
let
  yamllib = import ./yaml.nix { inherit pkgs; };
  k9s-plugins = pkgs.fetchgit {
    url = "https://github.com/derailed/k9s";
    rev = "e83cf33373a0e0576592ff1ca917ae30a42ffe69"; # Wed 19 Aug 2026 16:42:55 AEST

    sparseCheckout = [
      "plugins"
    ];
    hash = "sha256-izAIkcZ9xow5bI6hxrWRawbFxYSDHQbCcyJT+Qc0tn0=";
  };
  wanted-plugins = [
    "argo-workflows"
    "crd-wizard"
    "debug-container"
    # "dup"
    "flux"
    "get-all"
    "get-all-namespace-resources"
    "eks-node-viewer"
    "kube-metrics"
    "helm-diff"
    "helm-values"
    "log-stern"
    "log-lnav"
    "pvc-debug-container"
    "resource-recommendations"
    "watch-events"
  ];
  load-k9s-plugins = map (
    plugin-file: (yamllib.readYAML "${k9s-plugins}/plugins/${plugin-file}.yaml").plugins
  );

in
{

  programs.k9s = {
    enable = true;

    plugins = pkgs.lib.attrsets.mergeAttrsList (
      [
        {
          damien = {
            description = "Debug shell";
            shortCut = "Ctrl-X";
            scopes = [ "nodes" ];
            command = "sh";
            background = false;
            args = [
              "-c"
              ''
                INSTANCE_ID=$(kubectl get node $NAME -o=jsonpath='{.metadata.labels.instance-id}')
                kubectl debug node/$INSTANCE_ID -it --image=-amazonlinux:2

                # aws ssm start-session --target $INSTANCE_ID
              ''
            ];
          };
        }
      ]
      ++ (load-k9s-plugins wanted-plugins)
    );
  };

}
