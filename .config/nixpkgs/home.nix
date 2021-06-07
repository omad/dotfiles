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
  home.stateVersion = "20.09";

  home.username = "omad";
  home.homeDirectory = "/home/omad/";

#  services.lorri.enable = true;

  home.packages = with pkgs; [
    awscli2
    mdcat
    prettyping
    spotify-tui # rust spotify client
    tealdeer # rust tldr client
    fd # fast find alternative
    fzf
    ripgrep
    starship # minimal blazing fast prompt
    gitAndTools.delta
    jq
    htop
    direnv
    aws-vault
    bat
    fluxctl
    gitAndTools.hub
    kubectl
    kubectx
    nixfmt
#    python-language-server
    bottom
    scc
    kube3d
    exa
    git-secrets
    k9s
    rclone
    shellcheck
    terraform-docs
    gh
    pspg
    pgmetrics
    nodePackages.prettier
    dunst
    dive
  ];
}
