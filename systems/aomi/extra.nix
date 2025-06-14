{
  globals,
  ...
}:
{

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
    ../common/desktop/binfmt.nix # TODO: move to something else than desktop
    # ../common/services/buildkit.nix # maybe ?
    # ../common/services/oci-image-mirroring.nixi
    # ../common/services/ollama.nix # TODO handle nvidia vs not ?
    ../common/services/prometheus-exporters-node.nix
    # ../common/services/gitea-runner

    ../redhat
  ];

  # networking.firewall.enable = false;

  services = {
    logind.extraConfig = ''
      HandleLidSwitch=ignore
      HandleLidSwitchExternalPower=ignore
      HandleLidSwitchDocked=ignore
    '';
    wireguard = {
      enable = true;
      ips = globals.fn.wg-ips globals.machines.aomi.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
    };
    hardware.bolt.enable = true;
  };
}
