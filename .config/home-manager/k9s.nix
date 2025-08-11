{ pkgs, stdenv, lib, ... }:
let
  yamllib = import ./yaml.nix { inherit pkgs; };
  k9s-plugins = pkgs.fetchgit {
    url = "https://github.com/derailed/k9s";
    rev = "350439b98553f23672f7ce0b650637d0afdd4104";
    sparseCheckout = [
      "plugins"
    ];
    hash = "sha256-R14kvDAPKZOBLsFsnC9kAklJKYpdF1dNsl3YpCzQPrI=";
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
      }
    ] ++ (load-k9s-plugins wanted-plugins));
  };

}
