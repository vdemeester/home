{ pkgs, ... }:
{
  imports = [
    ../../home/common/dev/default.nix
    ../../home/common/dev/emacs.nix
    ../../home/common/dev/containers.nix
    ../../home/common/dev/tekton.nix
  ];
  services.ssh-agent.enable = true;
  systemd.user.services.syncthing.Install.WantedBy = [ "multi-user.target" ];

  home.packages = with pkgs; [
    gnumake
  ];
}
