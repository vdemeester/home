_: {

  imports = [
    ../common/hardware/laptop.nix
    ../common/programs/direnv.nix
    ../common/programs/git.nix
    ../common/programs/tmux.nix
    # ../common/services/networkmanager.nix
    # ../common/services/fprint.nix # With yubikey I don't really need this to be honest
    ../common/services/containers.nix
    ../common/services/docker.nix
    ../common/services/lxd.nix
    # ../common/services/ollama.nix # TODO handle nvidia vs not ?
    # ../common/services/prometheus-exporters
    # ../common/services/gitea-runner

    ../redhat
  ];

  services = {
    logind.extraConfig = ''
      HandleLidSwitch=ignore
      HandleLidSwitchExternalPower=ignore
      HandleLidSwitchDocked=ignore
    '';
    wireguard = {
      enable = true;
      ips = globals.fn.wg-ips globals.machines.kyushu.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.net.vpn.pubkey}";
    };
    hardware.bolt.enable = true;
  };
}
