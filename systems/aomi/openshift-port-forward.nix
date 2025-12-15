{ lib, ... }:

{
  # Enable IP forwarding for libvirt network
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = lib.mkForce 1;
    "net.ipv6.conf.all.forwarding" = lib.mkForce 1;
  };

  # Use nftables for better performance and cleaner syntax
  networking = {
    nftables = {
      enable = true;

      # IMPORTANT: Using tables instead of ruleset to allow libvirt to manage its own table
      # If we use ruleset, it completely replaces everything and libvirt can't create its table
      tables = {
        "nat" = {
          family = "ip";
          content = ''
            chain prerouting {
              type nat hook prerouting priority dstnat; policy accept;

              # Forward HTTP, HTTPS, and API traffic to OpenShift VM
              # Only from interfaces that are NOT virbr1 (to avoid loops)
              iifname != "virbr1" tcp dport 80 dnat to 192.168.100.7:80
              iifname != "virbr1" tcp dport 443 dnat to 192.168.100.7:443
              iifname != "virbr1" tcp dport 6443 dnat to 192.168.100.7:6443
            }

            chain postrouting {
              type nat hook postrouting priority srcnat; policy accept;

              # Masquerade traffic from libvirt network to external destinations
              ip saddr 192.168.100.0/24 ip daddr != 192.168.100.0/24 masquerade
            }

            chain output {
              type nat hook output priority dstnat; policy accept;

              # Forward localhost traffic destined for LAN IP to OpenShift VM
              ip daddr 192.168.1.23 tcp dport 80 dnat to 192.168.100.7:80
              ip daddr 192.168.1.23 tcp dport 443 dnat to 192.168.100.7:443
              ip daddr 192.168.1.23 tcp dport 6443 dnat to 192.168.100.7:6443
            }
          '';
        };
        "filter" = {
          family = "inet";
          content = ''
            chain input {
              type filter hook input priority filter; policy drop;

              # Allow established/related connections
              ct state { established, related } accept

              # Allow loopback
              iifname "lo" accept

              # Allow trusted interfaces
              iifname { "wg0", "docker0" } accept

              # Allow ICMP (ping)
              ip protocol icmp accept
              ip6 nexthdr ipv6-icmp accept

              # Allow SSH
              tcp dport 22 accept

              # Allow OpenShift ports
              tcp dport { 80, 443, 6443 } accept

              # Allow libvirt
              tcp dport 16509 accept

              # Allow mDNS
              udp dport 5353 accept
            }

            chain forward {
              type filter hook forward priority filter; policy accept;

              # Allow established/related connections
              ct state { established, related } accept

              # Allow forwarding to/from the libvirt OpenShift network
              ip daddr 192.168.100.0/24 accept
              ip saddr 192.168.100.0/24 accept
            }

            chain output {
              type filter hook output priority filter; policy accept;
            }
          '';
        };
      };

      # Old ruleset approach - completely removed
      # Using ruleset would prevent libvirt from managing its own table
      # See tables configuration above instead
    };

    # Disable the default NixOS firewall since we're using custom nftables ruleset
    firewall.enable = false;
  };
}
