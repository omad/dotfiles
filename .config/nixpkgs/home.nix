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
  home.stateVersion = "21.11";

  home.username = "omad";
  home.homeDirectory = "/home/omad/";

#  services.lorri.enable = true;

  home.packages = with pkgs; [
#    (callPackage (fetchTarball https://github.com/DavHau/mach-nix/tarball/3.4.0) {}).mach-nix
#    nixos-generators
    morph

    lftp
    tig

    upterm

    vale # Syntax aware prose linter

    comby # Structural code search and replace

    cloudflared

    nodePackages.insect # Nice calculator

    # nasc  # TODO uncomment not building 2022-09-14 # Another GUI calculator

    dasel

    gitui # fast cli git client https://github.com/extrawurst/gitui

    awscli2
    packer

    difftastic
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


    # Language Servers
    terraform-ls
    taplo
    rnix-lsp
    nodePackages.dockerfile-language-server-nodejs
    cmake-language-server

    kubectl
    kubectx
    kubeseal

    terraform

    zola # Rust Static Site Generator

    nixfmt
#    python-language-server
    bottom
    du-dust
    duf

    scc
    kube3d

    exa
    lsd

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
    pup
    docker-credential-helpers
    docker-compose
    docker-slim
    eksctl
    yt-dlp
    onefetch

    jless

    argo
    watchexec

    chezmoi
#    s5cmd
    niv
    mitmproxy

    btop

    tokei  # source lines of code counter

    goaccess  # Web Access Log Analyser

    kubernetes-helm

    hurl  # Rust wrapper for programmatic curl

    yq-go

#    hadolint  # Dockerfile linter

    ghq  # git repo manager

#    kakoune  # experimental better code editor

#    oil  # a new shell
#    elvish  # another new shell
#    nim  # a new programming language

    navi # interactive cli cheat sheets

    _1password

    nodePackages.prettier
    nodePackages.pyright
    nodePackages.yaml-language-server
#    nodePackages.aws-azure-login
  ];

  systemd.user.timers.odc-slack-export = {
      Unit = { Description = "Export ODC Slack"; };
      Timer = { 
        OnCalendar = "daily";
        Unit = "odc-slack-export.service";
      };
      Install = { WantedBy = [ "timers.target" ]; };
  };
  systemd.user.services.odc-slack-export = {
      Unit = { Description = "Export ODC Slack"; };
      Service = {
        EnvironmentFile = "/home/omad/dev/slack-export/token.env";
        WorkingDirectory = "/home/omad/dev/slack-export/";
        ExecStart = "/home/omad/dev/slack-export/.direnv/python-3.10.4/bin/python slack_export.py --token $SLACK_TOKEN";
      };
  };
  
#  systemd.user.services.theengs-gateway = with pkgs; let
#    TheengsGateway = callPackage ./theengs-gateway.nix {
#      pythonPackages = python3Packages;
#    };
#    TheengsEnv = python3.withPackages (ps: [TheengsGateway ]);
#  in {
#      Unit.Description = "Run Theengs Gateway BLE-MQTT gateway";
#      Service = {
#        ExecStart = "${TheengsEnv}/bin/python3 -m TheengsGateway -ll INFO";
#      };
#    };

}
