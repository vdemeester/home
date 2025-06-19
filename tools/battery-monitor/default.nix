{ buildGoModule }:

buildGoModule rec {
  name = "battery-monitor";
  src = ./.;
  # vendorHash = null;
  vendorHash = "sha256-K9lqUdIR2EpLEQMTrlv6bGj3IpwK/yaFCqY7S7nuXg4=";
}
