{
  globals,
  libx,
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
    ../common/services/libvirt.nix
    ../common/desktop/binfmt.nix # TODO: move to something else than desktop
    # ../common/services/buildkit.nix # maybe ?
    # ../common/services/oci-image-mirroring.nixi
    # ../common/services/ollama.nix # TODO handle nvidia vs not ?
    ../common/services/prometheus-exporters-node.nix
    # ../common/services/gitea-runner

    ../redhat
  ];

  networking.firewall.enable = false;

  # TODO make it an option ? (otherwise I'll add it for all)
  users.users.vincent.linger = true;

  services = {
    logind.settings.Login = {
      HandleLidSwitch = "ignore";
      HandleLidSwitchExternalPower = "ignore";
      HandleLidSwitchDocked = "ignore";
    };
    wireguard = {
      enable = true;
      ips = libx.wg-ips globals.machines.aomi.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
    };
    ollama = {
      enable = true;
      # acceleration = "cuda"; # no nivida :D
    };
    smartd = {
      enable = true;
      devices = [ { device = "/dev/nvme0n1"; } ];
    };
    hardware.bolt.enable = true;
    # gitea-actions-runner = {
    #   instances = {
    #     "aomi-codeberg" = {
    #       name = "aomi";
    #       enable = true;
    #       url = "https://codeberg.org";
    #       # tokenFile = "/home/vincent/sync/codeberg.token";
    #       tokenFile = "/etc/codeberg.token";
    #       labels = [
    #         # "local:host"
    #         "nixos-${pkgs.system}:host"
    #         "native:host"
    #         "docker:docker://gitea/runner-images:ubuntu-latest"
    #         "ubuntu-latest:docker://gitea/runner-images:ubuntu-latest"
    #         "ubuntu-24.04:docker://gitea/runner-images:ubuntu-24.04"
    #         "ubuntu-22.04:docker://gitea/runner-images:ubuntu-22.04"
    #         "ubuntu-20.04:docker://gitea/runner-images:ubuntu-20.04"
    #         # "nix:docker://localhost:5921/nix-runner"
    #       ];
    #       hostPackages = with pkgs; [
    #         bash
    #         direnv
    #         coreutils
    #         curl
    #         gawk
    #         nixVersions.stable
    #         gitFull
    #         gnused
    #         docker
    #         openssh
    #         wget
    #       ];
    #     };
    #   };
    # };
  };

}
