{ pkgs, stdenv, lib, ... }:
let
  yamllib = import ./yaml.nix { inherit pkgs; };
  k9s-plugins = pkgs.fetchgit {
      url = "https://github.com/derailed/k9s";
      sparseCheckout = [
        "plugins"
      ];
      hash = "sha256-sqcVBNPTi9tk2tQFf+xzgFbB81o+r+qZKPdrwgO1iSc=";
  };
  wanted-plugins = [ "flux" "debug-container" "get-all" "helm-values" "watch-events" ];
  load-k9s-plugins = map (
        plugin-file: 
            (yamllib.readYAML "${k9s-plugins}/plugins/${plugin-file}.yaml").plugins
        );

in
{

  programs.k9s = {
    enable = true;

    plugin = {
      plugins = pkgs.lib.attrsets.mergeAttrsList ([
        {
          damien = {
            description = "Shell to Node with AWS SSM";
            shortCut = "Ctrl-X";
            scopes = [ "nodes" ];
            command = "sh";
            background = false;
            args = [
              "-c"
              '' 
            INSTANCE_ID=$(kubectl get node $NAME -o=jsonpath='{.metadata.labels.instance-id}')
            aws ssm start-session --target $INSTANCE_ID
            ''
            ];
          };
        }] ++ (load-k9s-plugins wanted-plugins));
#        { dive = (yamllib.readYAML ./dive.yaml).plugins.dive; }
#       
  };
};

}
