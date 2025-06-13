{
  globals,
  ...
}:
{
  imports = [
    # ../common/services/gosmee.nix
    ../common/services/prometheus-exporters-node.nix
    # ../common/services/prometheus-exporters-nginx.nix
    # ../common/services/nginx.nix
    # ../common/services/govanityurl.nix
  ];

  services.wireguard.server = {
    enable = true;
    inherit (globals.machines.kerkouane.net.vpn) ips;
    peers = globals.fn.generateWireguardPeers globals.machines;
  };

  security.pam.enableSSHAgentAuth = true;
  services.govanityurl = {
    enable = true;
    user = "nginx";
    host = "go.sbr.pm";
    config = ''
      paths:
        /lord:
          repo: https://github.com/vdemeester/lord
        /ape:
          repo: https://git.sr.ht/~vdemeester/ape
        /nr:
          repo: https://git.sr.ht/~vdemeester/nr
        /ram:
          repo: https://git.sr.ht/~vdemeester/ram
        /sec:
          repo: https://git.sr.ht/~vdemeester/sec
    '';
  };
}
