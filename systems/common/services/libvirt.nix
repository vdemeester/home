{ lib, pkgs, ... }:
{
  system.nixos.tags = [ "libvirt" ];
  boot.kernelParams = [ "kvm_intel.nested=1" ];
  environment.etc."modprobe.d/kvm.conf".text = ''
    options kvm_intel nested=1
  '';
  virtualisation.libvirtd = {
    enable = true;
    allowedBridges = [ "br1" ]; # Could be different dependinng on the host ?
    firewallBackend = "nftables";
    extraConfig = ''
      listen_tls = 0
      listen_tcp = 1
      auth_tcp="none"
      tcp_port = "16509"
    '';
    # extraOptions = [ "--listen" ];
  };
  networking.firewall.allowedTCPPorts = [ 16509 ];
  security.polkit.enable = true; # 22.11: libvirtd requires poltkit to be enabled
  environment.systemPackages = with pkgs; [
    qemu
    vde2
    libosinfo
  ];
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = lib.mkDefault 1;
  };
}
