{
  globals,
  pkgs,
  ...
}:
{
  imports = [
    ../common/services/prometheus-exporters-node.nix
    ../common/services/containers.nix
    ../common/services/docker.nix
    ../common/services/lxd.nix
    ../common/services/libvirt.nix
  ];

  age.secrets."aria2RPCSecret" = {
    file = ../../secrets/shikoku/aria2rpcsecret.age;
    mode = "444";
    owner = "aria2";
    group = "aria2";
  };
  nixpkgs.config.permittedInsecurePackages = [
    "dotnet-sdk-6.0.428"
    "aspnetcore-runtime-6.0.36"
  ];

  services = {
    vscode-server.enable = true;
    wireguard = {
      enable = true;
      ips = globals.fn.wg-ips globals.machines.shikoku.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
    };
    aria2 = {
      enable = true;
      openPorts = true;
      # extraArguments = "--max-concurrent-downloads=20";
      settings = {
        max-concurrent-downloads = 20;
      };
      downloadDir = "/data/downloads";
      rpcSecretFile = "${pkgs.writeText "aria" "aria2rpc\n"}";
    };
    bazarr = {
      enable = true;
      # Use reverse proxy instead
      openFirewall = true;
    };
    radarr = {
      enable = true;
      # Use reverse proxy instead
      openFirewall = true;
    };
    sonarr = {
      enable = true;
      # Use reverse proxy instead
      openFirewall = true;
    };
    prowlarr = {
      enable = true;
      # Use reverse proxy instead
      openFirewall = true;
    };
    readarr = {
      enable = true;
      # Use reverse proxy instead
      openFirewall = true;
    };
    lidarr = {
      enable = true;
      # Use reverse proxy instead
      openFirewall = true;
    };
    smartd = {
      enable = true;
      devices = [ { device = "/dev/nvme0n1"; } ];
    };
    ollama = {
      enable = true;
      package = pkgs.ollama.override {
        config.cudaSupport = true;
        config.rocmSupport = false;
      };
      acceleration = "cuda"; # no nivida :D
    };
  };

  # Move this to a "builder" role, or maybe I don't need this anymore ?
  users.extraUsers.builder = {
    isNormalUser = true;
    uid = 1018;
    extraGroups = [ ];
    openssh.authorizedKeys.keys = [ (builtins.readFile ../../secrets/builder.pub) ];
  };
  nix.settings.trusted-users = [
    "root"
    "vincent"
    "builder"
  ];

  security.pam.sshAgentAuth.enable = true;
}
