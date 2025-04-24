{ pkgs, lib, ... }:
{

  imports = [
    ../common/hardware/laptop.nix
    ../common/programs/direnv.nix
    ../common/programs/git.nix
    ../common/programs/tmux.nix
    ../common/services/networkmanager.nix
    # ../common/services/fprint.nix # With yubikey I don't really need this to be honest
    ../common/services/containers.nix
    ../common/services/docker.nix
    ../common/services/lxd.nix

    ../redhat
  ];

  environment.systemPackages = with pkgs; [
    go-org-readwise
  ];

  # Make sure we don't start docker until required
  systemd.services.docker.wantedBy = lib.mkForce [ ];
  # Make sure we don't start lxd until required
  systemd.services.lxd.wantedBy = lib.mkForce [ ];

}
