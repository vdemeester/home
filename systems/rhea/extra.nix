{
  libx,
  globals,
  lib,
  pkgs,
  ...
}:
{
  users.users.vincent.linger = true;

  wireguard = {
    enable = true;
    ips = libx.wg-ips globals.machines.rhea.net.vpn.ips;
    endpoint = "${globals.net.vpn.endpoint}";
    endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
  };

  services.openssh.enable = true;
  services.openssh.settings = {
    PermitRootLogin = "yes";
    PasswordAuthentication = true;
  };

  networking.useDHCP = lib.mkDefault true;

  environment.systemPackages = with pkgs; [
    vim
    git
    htop
    helix
    # Add any additional packages you need.
  ];

}
