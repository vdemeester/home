{
  hostname,
  config,
  pkgs,
  lib,
  ...
}:
{
  imports = [
    ./boot.nix
    ./console.nix
    ./hardware.nix
    ./locale.nix
    ./nh.nix
    ./tpm.nix

    # All my machine have this running
    ../services/avahi.nix
    ../services/openssh.nix
    ../programs/age.nix
    # ../services/wireguard.nix # or netbird
  ];

  networking = {
    hostName = hostname;
    # useDHCP = lib.mkDefault true;
  };

  environment.systemPackages = with pkgs; [
    binutils
    curl
    f2
    file
    htop
    iotop
    killall
    lsof
    netcat
    pciutils
    psmisc
    pv
    ripgrep
    rsync
    traceroute
    tree
    usbutils
    vim
    wget
  ];

  programs = {
    zsh.enable = true;
  };

  services = {
    # Only keep the last 500MiB of systemd journal.
    journald.extraConfig = "SystemMaxUse=500M";
  };

  security = {
    polkit.enable = true;
    rtkit.enable = true;
  };

  # Clear out /tmp after a fortnight and give all normal users a ~/tmp
  # cleaned out weekly.
  systemd.tmpfiles.rules =
    [ "d /tmp 1777 root root 14d" ]
    ++ (
      let
        mkTmpDir = n: u: "d ${u.home}/tmp 0700 ${n} ${u.group} 7d";
      in
      lib.mapAttrsToList mkTmpDir (lib.filterAttrs (_: u: u.isNormalUser) config.users.extraUsers)
    );

}
