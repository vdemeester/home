{
  libx,
  pkgs,
  lib,
  globals,
  ...
}:
{

  imports = [
    ../common/hardware/laptop.nix
    ../common/programs/direnv.nix
    ../common/programs/git.nix
    ../common/programs/tmux.nix
    ../common/services/networkmanager.nix
    ../common/services/containers.nix
    ../common/services/docker.nix
    ../common/services/libvirt.nix

    ../redhat
  ];

  programs.obs-studio = {
    enable = true;
    plugins = with pkgs.obs-studio-plugins; [
      wlrobs
      obs-backgroundremoval
      obs-pipewire-audio-capture
      input-overlay
    ];
  };

  services = {
    getty = {
      autologinOnce = true;
      autologinUser = "vincent";
    };
    # TODO probably migrate elsewhere
    kanata = {
      enable = true;
      package = pkgs.kanata-with-cmd;
      keyboards.x1 = {
        devices = [ "/dev/input/event0" ]; # internal keyboard
        config = builtins.readFile (./. + "/main.kbd");
        extraDefCfg = ''
          	danger-enable-cmd yes
            process-unmapped-keys yes
            override-release-on-activation yes
            concurrent-tap-hold yes
        '';
      };
    };
    dictd = {
      enable = true;
      DBs = with pkgs.dictdDBs; [
        wiktionary
        wordnet
        fra2eng
        eng2fra
      ];
    };
    locate = {
      enable = true;
      pruneBindMounts = true;
    };
    # Dual-hub client: connect to both local hub (demeter) and remote hub (kerkouane)
    wireguard.dualClient = {
      enable = true;
      ips = libx.wg-ips globals.machines.kyushu.net.vpn.ips;

      # Local hub (demeter)
      localHub = {
        enable = true;
        endpoint = builtins.head globals.machines.${globals.net.localHub.host}.net.ips;
        endpointPort = globals.net.localHub.port;
        endpointPublicKey = globals.machines.${globals.net.localHub.host}.net.vpn.pubkey;
      };

      # Remote hub (kerkouane)
      remoteHub = {
        endpoint = globals.net.vpn.endpoint;
        endpointPort = globals.net.vpn.port;
        endpointPublicKey = globals.machines.kerkouane.net.vpn.pubkey;
      };
    };
    hardware.bolt.enable = true;
    printing = {
      enable = true;
      drivers = with pkgs; [
        cnijfilter2
        gutenprint
        gutenprintBin
      ];
    };
  };

  # XXX: it doesn't really work...
  # hardware.printers = {
  #   ensurePrinters = [
  #     {
  #       name = "Canon_MX530_series";
  #       location = "Home";
  #       deviceUri = "dnssd://Canon%20MX530%20series._ipp._tcp.local/?uuid=00000000-0000-1000-8000-D8492F0F31CC";
  #       model = "gutenprint.5.3://bjc-MX530-series/expert Canon MX530 series - CUPS+Gutenprint v5.3.4";
  #       # deviceUri = "http://192.168.178.2:631/printers/Dell_1250c";
  #       # model = "drv:///sample.drv/generic.ppd";
  #       ppdOptions = {
  #         PageSize = "A4";
  #       };
  #     }
  #   ];
  #   ensureDefaultPrinter = "Canon_MX530_series";
  # };

  hardware.keyboard.qmk.enable = true;

  services.udev.packages = [ pkgs.sane-airscan ];
  hardware.sane = {
    enable = true;
    extraBackends = [ pkgs.sane-airscan ];
    openFirewall = true;
    netConf = "192.168.12.70";
  };

  environment.systemPackages = with pkgs; [
    kanata
    nixos-rebuild-ng
    # go-org-readwise # FIXME: will add it back from its new place.
    battery-monitor
    # backup
    restic # TODO: will probably move this to it's own configuration some day
    virt-manager
  ];

  # Make sure we don't start docker until required
  systemd.services.docker.wantedBy = lib.mkForce [ ];
}
