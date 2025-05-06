{ modulesPath, ... }:

let
  hostname = "carthage";
in
{
  imports = [
    "${modulesPath}/virtualisation/amazon-image.nix"
    (import ../../users/vincent)
    (import ../../users/root)
  ];

  ec2.efi = true;

  networking.hostName = hostname;

  services = {
    openssh = {
      enable = true;
      startWhenNeeded = false;
    };
    sshguard.enable = true;
  };
  programs.mosh.enable = true;

}
