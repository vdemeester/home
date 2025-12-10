{ pkgs, ... }:
{
  system.nixos.tags = [ "ansible" ];

  environment.systemPackages = with pkgs; [
    ansible
    ansible-builder
    ansible-lint
  ];
}
