{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.03";

#  services.lorri.enable = true;

  home.packages = with pkgs; [
    awscli2
    mdcat
    kubectx
    prettyping
    spotify-tui # rust spotify client
    tealdeer # rust tldr client
    fd # fast find alternative
    starship # minimal blazing fast prompt
    gitAndTools.delta
    htop
    direnv
    aws-vault
    bat
    fluxctl
    gitAndTools.hub
    kubectl
    nixfmt
#    python-language-server
    bottom
    scc
    kube3d
    exa
    k9s
  ];
}
