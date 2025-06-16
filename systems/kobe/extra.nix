{
  globals,
  pkgs,
  lib,
  ...
}:
{
  imports = [
    ../common/services/prometheus-exporters-node.nix
    ../common/services/containers.nix
    ../common/services/docker.nix
    ../common/services/lxd.nix
  ];

  # networking.firewall.enable = false;
  nixpkgs.config.cudaSupport = true;
  nixpkgs.config.rocmSupport = lib.mkForce false;

  services = {
    logind.extraConfig = ''
      HandleLidSwitch=ignore
      HandleLidSwitchExternalPower=ignore
      HandleLidSwitchDocked=ignore
    '';
    wireguard = {
      enable = true;
      ips = globals.fn.wg-ips globals.machines.kobe.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
    };
    ollama = {
      enable = true;
      package = pkgs.ollama.override {
        config.cudaSupport = true;
        config.rocmSupport = false;
      };
      acceleration = "cuda"; # no nivida :D
    };
    smartd = {
      enable = true;
      devices = [ { device = "/dev/nvme0n1"; } ];
    };
  };

  # TODO: could be enable by default for all ?
  security.pam.enableSSHAgentAuth = true;

  security.apparmor.enable = true;
  security.tpm2.enable = lib.mkForce false;
}
