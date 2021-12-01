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
  home.stateVersion = "21.05";

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
    dunst
    dive
    jiq
    docker-credential-helpers
    pup
    docker-compose
    eksctl
    youtube-dl
    onefetch

    argo
    watchexec

    chezmoi
    oil
    s5cmd
    niv
    mitmproxy

    btop

    goaccess  # Web Access Log Analyser

    kubernetes-helm

    hurl  # Rust wrapper for programmatic curl

    yq-go

#    hadolint  # Dockerfile linter

    ghq  # git repo manager

    kakoune  # experimental better code editor

    oil  # a new shell
    elvish  # another new shell
    nim  # a new programming language

    navi # interactive cli cheat sheets

    _1password

    nodePackages.prettier
    nodePackages.pyright
    nodePackages.yaml-language-server
  ];
}
