{
  pkgs,
  ...
}:
let
  yamllib = import ./yaml.nix { inherit pkgs; };
  k9s-plugins = pkgs.fetchgit {
    url = "https://github.com/derailed/k9s";
    rev = "v0.50.18";
    sparseCheckout = [
      "plugins"
    ];
    hash = "sha256-zL1B7jgXCgNbB7k4B5vRMQLzrLZ+/36XEYm0uNtoefE=";
  };
  wanted-plugins = [
    "debug-container"
    "flux"
    "get-all"
    "helm-values"
    "watch-events"
    "resource-recommendations"
    "crd-wizard"
    "argo-workflows"
    "helm-diff"
    "log-stern"
    "dup"
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
