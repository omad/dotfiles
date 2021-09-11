{ modulesPath, pkgs, ... }: {
  imports = [ "${modulesPath}/virtualisation/amazon-image.nix" ];
  ec2.hvm = true;

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
  programs.neovim = {
    enable = true;
    defaultEditor = true;
    viAlias = true;
    vimAlias = true;
  };
  environment.systemPackages = with pkgs; [ 
    neovim
    file
    git
    nix-prefetch-scripts
    wget
    curl
    which
    ripgrep
    yadm
  ];

  programs.iftop.enable = true;
  programs.iotop.enable = true;
  programs.tmux.enable = true;

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };
  users.users.omad = {
    uid = 1000;
    isNormalUser = true;
    home = "/home/omad";
    description = "Damien Ayers";
    extraGroups = [ "wheel" "docker" ];
    openssh.authorizedKeys.keys = [
                                    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPACrl48JaQJvv9/yr6KAqYAbEwrG2CEYbZW32IGiAV3 work-surface-ed25519-key-20190802"
                                    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDR1CcJQlss+TUgmtkilmtafV4chjXk0Yk1ec/dmWREU omad@Damiens-MacBook-Pro.local"
    ];
  };
  time.timeZone = "Australia/Canberra";

  services.openssh.forwardX11 = true;

#  fonts.fonts = with pkgs; [
#    # Both needed to have ligatures work with doom-emacs.
#    fira-code-symbols
#    iosevka
#
##    input-fonts
#    inconsolata
#    arphic-ukai
#    dejavu_fonts
#    emacs-all-the-icons-fonts
#    noto-fonts-cjk
#  ];

#  virtualisation.docker = {
#    enable = true;
#    listenOptions = [ "/var/run/docker.sock" "2375" ];
#  };
  nixpkgs.config.allowUnfree = true;
#  systemd.services.dailyshutdown = {
#    description = "Shut down daily outside of work hours";
#    startAt = "18:00:00";
#    serviceConfig = {
#      Type = "oneshot";
#      ExecStart = "/run/current-system/sw/bin/shutdown +5";
#    };
#    wantedBy = ["default.target"];
#  };

  networking.firewall = {
    allowedTCPPorts = [ 17500 ];
    allowedUDPPorts = [ 17500 ];
  };

}
