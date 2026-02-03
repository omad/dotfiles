{ pkgs, config, ... }:

{

  nixpkgs.config = {
    allowUnfree = true;
  };
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  # golang terminal file manager
  programs.lf.enable = true;

  programs.bat = {
    enable = true;
    config.theme = "Dracula";
    # extraPackages = with pkgs.bat-extras; [
    #   batdiff
    #   batman
    #   prettybat
    # ];
  };

  programs.nh = {
    enable = true;
    clean.enable = true;
    clean.extraArgs = "--keep 5 --keep-since 3d";
    homeFlake = "${config.home.homeDirectory}/.config/home-manager/";

  };

  # programs.neovim = {
  #   enable = true;
  #   vimAlias = true;
  #   vimdiffAlias = true;
  # };

  programs.zoxide = {
    enable = true;
    enableFishIntegration = true;
    options = ["--cmd" "cd"];
  };

#   nixpkgs.overlays = [
# #    (final: prev: {
# #      granted = prev.granted.override {
# #        withFish = true;
# #      };
# #    })
#     (final: prev: {
#       s5cmd = prev.s5cmd.overrideAttrs (finalAttrs: previousAttrs: {
#         ldflags = [
#           "-X github.com/peak/s5cmd/v2/version.Version=${previousAttrs.version}"
#           "-X github.com/peak/s5cmd/v2/version.GitCommit=${previousAttrs.version}"
#         ];
#       });
#     })
#   ];

  # Fish Completions for `nix` and `home-manager`
  # Why isn't this setup automatically? Doing it this way is awfully hacky.
  #
  # Okay, so, this *is* hacky. It actually looks more like nix and home-manager should be including themselves in XDG_DATA_DIRS.
  # From there, fish populates $__fish_vendor_completionsdirs
  # And it also looks for completions in $fish_complete_path
  xdg.configFile."fish/completions/nix.fish".source = "${pkgs.nix}/share/fish/vendor_completions.d/nix.fish";
  xdg.configFile."fish/completions/home-manager.fish".source = "${pkgs.home-manager}/share/fish/vendor_completions.d/home-manager.fish";

  # Better ls
  programs.lsd.enable = true;

  # Advanced Shell History + Syncing
  programs.atuin.enable = true;

  programs.fish = {
    enable = true;
    shellAbbrs = {
      hm = "home-manager";
      mm = "micromamba";
      g = "git";
      k = "kubectl";
    };

    shellInit = ''
        # >>> mamba initialize >>>
        # !! Contents within this block are managed by 'mamba init' !!
        set -gx MAMBA_EXE "$HOME/.local/bin/micromamba"
        set -gx MAMBA_ROOT_PREFIX "$HOME/micromamba"
        $MAMBA_EXE shell hook --shell fish --root-prefix $MAMBA_ROOT_PREFIX | source
        # <<< mamba initialize <<<

        fnm env --shell fish | source

        # Set up the bun js tool
        if test -d "$HOME/.bun"
          set --export BUN_INSTALL "$HOME/.bun"
          set --append PATH "$BUN_INSTALL/bin"
        end
      '';
      interactiveShellInit = ''
        # Set up the granted/assume alias
        # See https://docs.commonfate.io/granted/internals/shell-alias
        alias assume="source (brew --prefix)/bin/assume.fish"

        # WTF is this not managed by home-manager!?
        # https://github.com/nix-community/home-manager/issues/5119
        # Closed PR: https://github.com/nix-community/home-manager/pull/5199
        # [fish: let plugin read vendor_* dirs which is used in nixpkgs fishPlugins by Vonfry · Pull Request #5237 · nix-community/home-manager](https://github.com/nix-community/home-manager/pull/5237)
        set nix_profile_fish ~/.nix-profile/share/fish

        # Add nix profile completions
        for dir in completions generated_completions vendor_completions.d
          if test -d "$nix_profile_fish/$dir"
            set --append fish_complete_path "$nix_profile_fish/$dir"
          end
        end

        if test -d $nix_profile_fish/vendor_functions.d
          # set fish_function_path $fish_function_path[1] $nix_profile_fish/vendor_functions.d $fish_function_path[2..-1]
          set --append fish_function_path "$nix_profile_fish/vendor_functions.d"
        end
        if test -d $nix_profile_fish/vendor_completions.d
          # set fish_complete_path $fish_complete_path[1] $nix_profile_fish/vendor_completions.d $fish_complete_path[2..-1]
          set --append fish_complete_path "$nix_profile_fish/vendor_completions.d"
        end

        # Source initialization code if it exists.
        if test -d $nix_profile_fish/vendor_conf.d
          for f in $nix_profile_fish/vendor_conf.d/*.fish
            source $f
          end
        end

        # Kubectl krew
        if set -q KREW_ROOT; and test -d "$KREW_ROOT/.krew/bin"

          set --append PATH "$KREW_ROOT/.krew/bin"
        else if test -d $HOME/.krew/bin
          set --append PATH "$HOME/.krew/bin"
        end
        # set -q KREW_ROOT; and ; or set -gx PATH $PATH $HOME/.krew/bin


      '';
    plugins = [
      {
        name = "fzf.fish";
        src = pkgs.fetchFromGitHub {
          owner = "PatrickF1";
          repo = "fzf.fish";
          rev = "8920367cf85eee5218cc25a11e209d46e2591e7a";
          sha256 = "sha256-T8KYLA/r/gOKvAivKRoeqIwE2pINlxFQtZJHpOy9GMM=";
        };
      }
    ];
  };
  # This conflicts with the pop-os installed glib and mime type associations
  # creating and infinite loop and crash. Something with
  # ecmascript and x-perl types.
  # Debugged with:
  #   $ DEBUGINFOD_URLS="https://debuginfod.ubuntu.com" gdb nautilus
  xdg.mime.enable = false;

  # TUI File Manager
  programs.xplr.enable = true;

  programs.jujutsu = {
    # disabling because I want latest versions
    enable = false;
    settings = {
      user = {
        name = "Damien Ayers";
        email = "damien@omad.net";
      };
    };
  };

  programs.helix = {
    enable = true;
    defaultEditor = true;
  };

#  programs.granted.enable = true;
#  programs.fish.shellAliases = {
#    assume = "source ${pkgs.granted}/share/assume.fish";
#  };

  # Rust TLDR client
  # programs.tealdeer = {
  #   enable = true;
  #   settings = {
  #     updates = {
  #       auto_update = true;
  #     };
  #   };
  # };

  xsession.enable = false;

  # Not sure what this fixes, but it probably breaks things too
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

  home.username = "aye011";
  home.homeDirectory = "/Users/aye011";

  # Extra Paths to always set
  home.sessionPath = [
    "$HOME/.local/bin"
    "$HOME/bin"
    "$HOME/go/bin"
    "$HOME/.pixi/bin"
    "$HOME/.cargo/bin"
    "/opt/homebrew/bin"
  ];

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

  programs.git.enable = true;
  programs.git.lfs = {
    enable = true;
  };


  #  services.lorri.enable = true;
  manual.manpages.enable = true;

  home.packages = with pkgs; [
    #    (callPackage (fetchTarball https://github.com/DavHau/mach-nix/tarball/3.4.0) {}).mach-nix
    #    nixos-generators
    morph

    # nushell
    usql


    ast-grep

    fastfetch

    nix-tree
    xh
#    numbat
    #    scrcpy

    # I tried git from here because the pop-os deb install was crashing due to the envsubst version
    # being differet
    # git

    # Convert lots of standard command output to JSON
    jc

    # Go tool to pin GitHub Actions shas
    pinact

    # jira-cli-go

    # Log Highlighter
#    ccze # No aarch64, 2024-11-20

    hyperfine

    jsonnet-bundler
    ijq

    # Benchmarking utilities
    hey
    k6

    grafana-loki

    git-filter-repo

    #pandoc
    #    duckdb

    #    miller # Like awk, sed, cut, join, and sort (or jq, csvkit, xsv) for data formats such as CSV, TSV, JSON, JSON Lines, and positionally-indexed

    sops
    age

    atac # HTTP/Rest TUI (rust)


    lazygit
    lazydocker

    just # command runner

    #    envsubst  # The a8m go implementation, not the gnu gettext one
    prometheus # For promtool

    glow # tui markdown reader

    lftp
    tig

    bfs # Breadth first find alternative


    mqttui

    nix-init

    # gitui #build fails

    asciinema

    btop

    trivy

    upterm

    mosquitto

    driftctl

    # ruff # Installed via uvx

    skopeo # container registry sync tool

    cue # cue language

    vale # Syntax aware prose linter

    # Broken on aarch64 2024-11-20, ocaml-mirage-rng
    # comby # Structural code search and replace

    # cloudflared

    # nasc  # TODO uncomment not building 2022-09-14 # Another GUI calculator

    dasel

    # gitui # fast cli git client https://github.com/extrawurst/gitui

    awscli2
    aws-sso-cli
    # aws-sam-cli

    # Ugh, this failed compiling on 2026-02-02
    # ssm-session-manager-plugin
    # eksctl

    difftastic
    mdcat
    prettyping
    # spotify-tui # rust spotify client
    fd # fast find alternative
    fzf # fuzzyfinder
    ripgrep
    starship # minimal blazing fast prompt
    delta
    jq
    fx # interactive jq
    jiq # interactive jq
    htop

    # Language Servers
    # LanguageTool LSP server
    # Supports markdown, rst, grammar/spelling/syntax checking
    ltex-ls
    # terraform-ls
    tflint
    taplo
    # nodePackages.dockerfile-language-server-nodejs
    lua-language-server
    nodePackages.vscode-json-languageserver
    typescript-language-server
    yaml-language-server
    cmake-language-server
    # docker-compose-language-service
    bash-language-server

    # Broken as of August 2025
    # autotools-language-server  # Also include make-language-server

    nil
    nixd
    # marksman
    # harper # Grammar checking language server
#    python3Packages.python-lsp-server

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
    argo-workflows
    kubernetes-helm

    #    terraform
    terraform-docs

    zola # Rust Static Site Generator

    nixfmt
    bottom
    # du-dust
    dust
    duf

    scc
    # kube3d
    k3d

    # eza
    lsd

    git-secrets
    rclone
    shellcheck
    gh
    pspg
    pgmetrics
    # dive
    pup
    docker-credential-helpers
    docker-compose
    docker-slim
    # yt-dlp # Installed via brew now
    onefetch

    jless

    watchexec

    # s5cmd
    niv
#    mitmproxy
    rustscan

    btop

    tokei # source lines of code counter

    goaccess # Web Access Log Analyser


    #    hurl # Rust wrapper for programmatic curl

    yq-go # Like jq, but for yaml

    #    hadolint  # Dockerfile linter

    ghq # git repo manager

    #    kakoune  # experimental modal code editor

    #    oil  # a new shell
    #    elvish  # another new shell
    #    nim  # a new programming language

    navi # interactive cli cheat sheets

    #    _1password

    # Disable 2025-02-15 to get Zed to work again
    # pyright

    nodePackages.prettier
    #    nodePackages.aws-azure-login
  ];

  systemd.user.paths.watch-download-torrents = {
    Unit = { Description = "Watch Downloads"; };
    Path = {
      PathChanged = "/Users/aye011/Downloads/";
      #           PathExistsGlob = "/home/aye011/Downloads/*.torrent";
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
        WorkingDirectory = "/Users/aye011/Downloads/";
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
