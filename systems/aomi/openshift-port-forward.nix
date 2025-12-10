{ ... }:

{
  # Enable IP forwarding for libvirt network
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = 1;
    "net.ipv6.conf.all.forwarding" = 1;
  };

  networking.firewall = {
    enable = true;

    # Open ports that will be forwarded to OpenShift
    allowedTCPPorts = [
      443 # HTTPS - OpenShift console and apps
      6443 # Kubernetes API
    ];

    # NAT rules to forward traffic to OpenShift VM
    extraCommands = ''
      # Get the primary home network interface
      HOME_IFACE=$(ip route | grep default | awk '{print $5}' | head -1)

      # Forward HTTPS traffic (443) to OpenShift
      iptables -t nat -A PREROUTING -i $HOME_IFACE -p tcp --dport 443 -j DNAT --to-destination 192.168.100.7:443

      # Forward Kubernetes API (6443) to OpenShift
      iptables -t nat -A PREROUTING -i $HOME_IFACE -p tcp --dport 6443 -j DNAT --to-destination 192.168.100.7:6443

      # Enable masquerading for libvirt network to access internet
      iptables -t nat -A POSTROUTING -s 192.168.100.0/24 ! -d 192.168.100.0/24 -j MASQUERADE

      # Allow forwarding between interfaces
      iptables -A FORWARD -i $HOME_IFACE -o virbr1 -j ACCEPT
      iptables -A FORWARD -i virbr1 -o $HOME_IFACE -j ACCEPT
    '';

    extraStopCommands = ''
      HOME_IFACE=$(ip route | grep default | awk '{print $5}' | head -1)

      # Clean up forwarding rules
      iptables -t nat -D PREROUTING -i $HOME_IFACE -p tcp --dport 443 -j DNAT --to-destination 192.168.100.7:443 2>/dev/null || true
      iptables -t nat -D PREROUTING -i $HOME_IFACE -p tcp --dport 6443 -j DNAT --to-destination 192.168.100.7:6443 2>/dev/null || true
      iptables -t nat -D POSTROUTING -s 192.168.100.0/24 ! -d 192.168.100.0/24 -j MASQUERADE 2>/dev/null || true
    '';
  };
}
