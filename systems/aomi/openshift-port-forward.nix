{ lib, ... }:

{
  # Enable IP forwarding for libvirt network
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = lib.mkForce 1;
    "net.ipv6.conf.all.forwarding" = lib.mkForce 1;
  };

  networking.firewall = {
    enable = true;

    # Open ports that will be forwarded to OpenShift
    allowedTCPPorts = [
      443 # HTTPS - OpenShift console and apps
      6443 # Kubernetes API
    ];

    # NAT rules to forward traffic to OpenShift VM
    # Using -i ! virbr1 to match all interfaces except the libvirt bridge
    extraCommands = ''
      # Forward HTTPS traffic (443) to OpenShift from any external interface
      iptables -t nat -A PREROUTING ! -i virbr1 -p tcp --dport 443 -j DNAT --to-destination 192.168.100.7:443

      # Forward Kubernetes API (6443) to OpenShift from any external interface
      iptables -t nat -A PREROUTING ! -i virbr1 -p tcp --dport 6443 -j DNAT --to-destination 192.168.100.7:6443

      # Forward from localhost (for commands running on aomi itself)
      iptables -t nat -A OUTPUT -p tcp --dport 443 -d 192.168.1.23 -j DNAT --to-destination 192.168.100.7:443
      iptables -t nat -A OUTPUT -p tcp --dport 6443 -d 192.168.1.23 -j DNAT --to-destination 192.168.100.7:6443

      # Enable masquerading for libvirt network to access internet
      iptables -t nat -A POSTROUTING -s 192.168.100.0/24 ! -d 192.168.100.0/24 -j MASQUERADE

      # Allow forwarding to/from the libvirt network
      iptables -A FORWARD -d 192.168.100.0/24 -j ACCEPT
      iptables -A FORWARD -s 192.168.100.0/24 -j ACCEPT
    '';

    extraStopCommands = ''
      # Clean up forwarding rules
      iptables -t nat -D PREROUTING ! -i virbr1 -p tcp --dport 443 -j DNAT --to-destination 192.168.100.7:443 2>/dev/null || true
      iptables -t nat -D PREROUTING ! -i virbr1 -p tcp --dport 6443 -j DNAT --to-destination 192.168.100.7:6443 2>/dev/null || true
      iptables -t nat -D OUTPUT -p tcp --dport 443 -d 192.168.1.23 -j DNAT --to-destination 192.168.100.7:443 2>/dev/null || true
      iptables -t nat -D OUTPUT -p tcp --dport 6443 -d 192.168.1.23 -j DNAT --to-destination 192.168.100.7:6443 2>/dev/null || true
      iptables -t nat -D POSTROUTING -s 192.168.100.0/24 ! -d 192.168.100.0/24 -j MASQUERADE 2>/dev/null || true
      iptables -D FORWARD -d 192.168.100.0/24 -j ACCEPT 2>/dev/null || true
      iptables -D FORWARD -s 192.168.100.0/24 -j ACCEPT 2>/dev/null || true
    '';
  };
}
