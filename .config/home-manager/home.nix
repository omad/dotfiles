{ pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  #  programs.neovim = {
  #    enable = true;
  #    vimAlias = true;
  #    vimdiffAlias = true;
  #
  #  };


  # This conflicts with the pop-os installed glib and mime type associations
  # creating and infinite loop and crash. Something with
  # ecmascript and x-perl types. 
  # Debugged with:
  #   $ DEBUGINFOD_URLS="https://debuginfod.ubuntu.com" gdb nautilus
  xdg.mime.enable = false;

  # TUI File Manager
  programs.xplr.enable = true;

  programs.jujutsu = {
    enable = true;
    settings = {
      user = {
        name = "Damien Ayers";
        email = "damien@omad.net";
      };
    };
  };

  programs.helix.enable = true;

  programs.granted.enable = true;

  programs.taskwarrior = {
    enable = true;
  };

  # Rust TLDR client
  programs.tealdeer.enable = true;

  xsession.enable = false;

  # Not sure what this fixes, but it probably breaks thigns too
  targets.genericLinux.enable = false;

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

  #  services.flameshot.enable = true;
  #  services.sxhkd = {
  #      enable = true;
  #      keybindings = {
  #          "F1" = "jumpapp -m kitty";
  #      };
  #  };
  # Mock tray for running my own WM/session
  # See https://github.com/nix-community/home-manager/issues/2064
  #  systemd.user.targets.tray = {
  #          Unit = {
  #                  Description = "Home Manager System Tray";
  #                  Requires = [ "graphical-session-pre.target" ];
  #          };
  #  };
  #    systemd.user.services.mock-tray = {
  #      Unit = {
  #        Description = "Mock X Tray";
  #        PartOf = [ "tray.target" ];
  #      };
  #
  #      Service = {
  #        Type = "oneshot";
  #        ExecStart = "${pkgs.coreutils}/bin/true";
  #      };
  #
  #      Install.WantedBy = [ "graphical-session.target" ];
  #    };

  #  services.lorri.enable = true;
  manual.manpages.enable = true;

  home.packages = with pkgs; [
    #    (callPackage (fetchTarball https://github.com/DavHau/mach-nix/tarball/3.4.0) {}).mach-nix
    #    nixos-generators
    (callPackage ./fastgron.nix { })
    morph

    nushell
    usql


    fastfetch

    xh
    numbat
    #    scrcpy

    # I tried git from here because the pop-os deb install was crashing due to the envsubst version
    # being differet
    git

    # Convert lots of standard command output to JSON
    jc

    jira-cli-go

    # Log Highlighter
    ccze

    hyperfine

    jsonnet-bundler
    ijq

    hey

    hugo

    #    granted

    grafana-loki

    pandoc
    duckdb

    miller # Like awk, sed, cut, join, and sort (or jq, csvkit, xsv) for data formats such as CSV, TSV, JSON, JSON Lines, and positionally-indexed

    sops
    age

    atuin

    lazygit
    lazydocker

    just # command runner

    #    envsubst  # The a8m go implementation, not the gnu gettext one
    prometheus # For promtool

    glow # tui markdown reader

    lftp
    tig

    bfs # Breadth first find alternative

    ltex-ls

    mqttui

    nix-init

    gitui

    asciinema

    btop

    trivy

    upterm
    #    wine
    #    winetricks

    mosquitto

    driftctl

    ruff

    skopeo # container registry sync tool

    cue # cue language

    vale # Syntax aware prose linter

    comby # Structural code search and replace

    cloudflared

    # nasc  # TODO uncomment not building 2022-09-14 # Another GUI calculator

    dasel

    gitui # fast cli git client https://github.com/extrawurst/gitui

    awscli2
    aws-sso-cli
    aws-sam-cli
    ssm-session-manager-plugin
    eksctl

    difftastic
    mdcat
    prettyping
    # spotify-tui # rust spotify client
    fd # fast find alternative
    fzf  # fuzzyfinder
    ripgrep
    starship # minimal blazing fast prompt
    gitAndTools.delta
    jq
    fx  # interactive jq
    jiq # interactive jq
    htop
    bat


    # Language Servers
    terraform-ls
    tflint
    taplo
    nodePackages.dockerfile-language-server-nodejs
    cmake-language-server
    docker-compose-language-service
    #    python-language-server

    # Kubernetes tools
    krew
    kubectl
    kubectl-convert
    kubectx
    kubecolor
    kubeseal
    kustomize
    kubeconform
    minikube
    kubeswitch

    # The latest versions of flux aren't backwards compatible
    # So I've downlaoded a binary from GitHub Releases
    #    fluxcd
    argo
    kubernetes-helm

    #    terraform
    terraform-docs

    zola # Rust Static Site Generator

    nixfmt-classic
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
    dive
    pup
    docker-credential-helpers
    docker-compose
    docker-slim
    yt-dlp
    onefetch

    jless

    watchexec

    s5cmd
    niv
    mitmproxy
    rustscan

    btop

    tokei # source lines of code counter

    goaccess # Web Access Log Analyser


        #    hurl # Rust wrapper for programmatic curl

    yq-go  # Like jq, but for yaml

    #    hadolint  # Dockerfile linter

    ghq # git repo manager

    #    kakoune  # experimental modal code editor
    #    helix

    #    oil  # a new shell
    #    elvish  # another new shell
    #    nim  # a new programming language

    navi # interactive cli cheat sheets

    #    _1password

    pyright

    nodePackages.prettier
    yaml-language-server
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
  systemd.user.services.watch-download-torrents =
    let
      script = pkgs.writeScript "watch-download-torrents" ''
        #!${pkgs.fish}/bin/fish --no-config
        echo Download Manager Triggered

        for f in *.torrent
           echo Adding $f
           ${pkgs.transmission_4}/bin/transmission-remote nixos --add "$f"
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
  systemd.user.timers.cleanup-caches = {
    Unit = { Description = "Cleanup Caches"; };
    Timer = {
      OnCalendar = "daily";
      Unit = "cleanup-caches.service";
    };
    Install = { WantedBy = [ "timers.target" ]; };
  };
  systemd.user.services.cleanup-caches = {
    Unit = { Description = "Cleanup Caches"; };
    Service = {
      ExecStart = "docker system prune --force";
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
