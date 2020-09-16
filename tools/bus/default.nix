{ stdenv, lib, buildGoModule }:

buildGoModule rec {
  name = "bus";
  src = ./.;

  vendorSha256 = "05iqr46pm05jxfcss23cxdqng1wpnqp46v5vyw8f65sq0vsfv4xh";
}
