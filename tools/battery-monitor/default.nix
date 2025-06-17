{ buildGoModule }:

buildGoModule rec {
  name = "battery-monitor";
  src = ./.;
  # vendorHash = null;
  vendorHash = "sha256-qWQM+DlW/9/JlAv9M7QBUch3wSeOQFVgGxmkIm29yAM=";
}
