{ config, pkgs, args, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

#  programs.neovim = {
#    enable = true;
#    vimAlias = true;
#    vimdiffAlias = true;
#
#  };


  targets.genericLinux.enable = true;

  # avoid redownloding and re-evaluating nixpkgs every time I
  # do a 'nix search' or 'nix shell'
#  nix.registry.nixpkgs = {
#      from = { type = "indirect"; id = "nixpkgs"; };
#      flake = args.pkgs;
#  };

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
  manual.manpages.enable = false;

  home.packages = with pkgs; [
#    (callPackage (fetchTarball https://github.com/DavHau/mach-nix/tarball/3.4.0) {}).mach-nix
#    nixos-generators
    (callPackage ./fastgron.nix {})
    morph

    nushellFull
    usql

    # Convert lots of standard command output to JSON
    jc

    jira-cli-go

    hyperfine

    jsonnet-bundler

    neovide

    hugo

    duckdb

    lazygit
    lazydocker

    kubeconform

    glow # tui markdown reader

    lftp
    tig

    bfs # Breadth first find alternative

    ltex-ls

    mqttui

    nix-init

    gitui

    qpwgraph

    asciinema

    btop

    upterm
    wine
    winetricks

    mosquitto

    driftctl

    ruff

    skopeo # container registry sync tool

    cue # cue language

    vale # Syntax aware prose linter

    comby # Structural code search and replace

    cloudflared

    nodePackages.insect # Nice calculator

    # nasc  # TODO uncomment not building 2022-09-14 # Another GUI calculator

    dasel

    gitui # fast cli git client https://github.com/extrawurst/gitui

    awscli2
    ssm-session-manager-plugin
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


    gitAndTools.hub


    # Language Servers
    terraform-ls
    tflint
    taplo
    rnix-lsp
    nodePackages.dockerfile-language-server-nodejs
    cmake-language-server
    docker-compose-language-service
#    python-language-server

    kubectl
    kubectl-convert
    kubectx
    kubecolor
    kubeseal
    kustomize
    fluxctl
    fluxcd
    k9s
    argo
    kubernetes-helm

    terraform
    terraform-docs

    zola # Rust Static Site Generator

    nixfmt
    bottom
    du-dust
    duf

    scc
    kube3d

#    exa No longer supported apparently
    lsd

    git-secrets
    rclone
    shellcheck
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

    watchexec

    s5cmd
    niv
    mitmproxy
    rustscan

    btop

    tokei  # source lines of code counter

    goaccess  # Web Access Log Analyser


    hurl  # Rust wrapper for programmatic curl

    yq-go

#    hadolint  # Dockerfile linter

    ghq  # git repo manager

#    kakoune  # experimental better code editor
    helix

#    oil  # a new shell
#    elvish  # another new shell
#    nim  # a new programming language

    navi # interactive cli cheat sheets

#    _1password

    nodePackages.prettier
    nodePackages.pyright
    nodePackages.yaml-language-server
#    nodePackages.aws-azure-login
  ];

   systemd.user.paths.watch-download-torrents = {
       Unit = { Description = "Watch Downloads"; };
       Path = {
           PathChanged = "/home/omad/Downloads/";
#           PathExistsGlob = "/home/omad/Downloads/*.torrent";
           Unit = "watch-download-torrents.service";
       };
      Install = { WantedBy = [ "paths.target" ]; };
 
   };
   systemd.user.services.watch-download-torrents = let
       script = pkgs.writeScript "watch-download-torrents" ''
           #!${pkgs.fish}/bin/fish
           echo Download Manager Triggered

           for f in *.torrent
              echo Adding $f
              ${pkgs.transmission}/bin/transmission-remote nixos --add "$f"
              and rm "$f"
           end
           '';
   in
     {
       Unit = { Description = "Act on Downloaded File"; };
       Service = {
           WorkingDirectory = "/home/omad/Downloads/";
           ExecStart = "${script}";
 #          ExecStart = "";
       };
   };
#  systemd.user.timers.odc-slack-export = {
#      Unit = { Description = "Export ODC Slack"; };
#      Timer = { 
#        OnCalendar = "daily";
#        Unit = "odc-slack-export.service";
#      };
#      Install = { WantedBy = [ "timers.target" ]; };
#  };
#  systemd.user.services.odc-slack-export = {
#      Unit = { Description = "Export ODC Slack"; };
#      Service = {
#        EnvironmentFile = "/home/omad/dev/slack-export/token.env";
#        WorkingDirectory = "/home/omad/dev/slack-export/";
#        ExecStart = "/home/omad/dev/slack-export/.direnv/python-3.10.4/bin/python slack_export.py --token $SLACK_TOKEN";
#      };
#  };
  
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
