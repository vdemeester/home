{ config, lib, pkgs, ... }:

let
  gpgRemoteForward = {
    bind.address = "/run/user/1000/gnupg/S.gpg-agent";
    host.address = "/run/user/1000/gnupg/S.gpg-agent.extra";
  };
  gpgSSHRemoteForward = {
    bind.address = "/run/user/1000/gnupg/S.gpg-agent.ssh";
    host.address = "/run/user/1000/gnupg/S.gpg-agent.ssh";
  };

  inherit (lib) optionalAttrs importTOML hasAttr attrsets mkIf;
  metadata = importTOML ../../../ops/hosts.toml;

  hasWireguard = name: value: hasAttr "wireguard" value;
  hasAddrs = name: value: hasAttr "addrs" value;
  hasSShAndRemoteForward = v: (hasAttr "ssh" v) && (hasAttr "gpgRemoteForward" v.ssh);

  hostWireguardIP = v: "${v.wireguard.addrs.v4}";
  hostIP = v: "${v.addrs.v4}";

  hostToSSHConfigItem = value: ipfn: {
    hostname = ipfn value;
    remoteForwards = mkIf (hasSShAndRemoteForward value) [ gpgRemoteForward gpgSSHRemoteForward ];
  };
  hostToSSHConfig = suffix: ipfn:
    name: value: attrsets.nameValuePair
      (toString "${name}${suffix}")
      (hostToSSHConfigItem value ipfn);

  vpnConfig = attrsets.mapAttrs'
    (hostToSSHConfig "\.vpn" hostWireguardIP)
    (attrsets.filterAttrs hasWireguard metadata.hosts);
  homeConfig = attrsets.mapAttrs'
    (hostToSSHConfig "\.home" hostIP)
    (attrsets.filterAttrs hasAddrs metadata.hosts);
in
{
  home.packages = [
    pkgs.openssh
  ];
  home.file.".ssh/sockets/.placeholder".text = '''';
  xdg.configFile."ssh/.placeholder".text = '''';
  programs.ssh = {
    enable = true;

    serverAliveInterval = 60;
    hashKnownHosts = true;
    userKnownHostsFile = "${config.xdg.configHome}/ssh/known_hosts";
    controlMaster = "auto";
    controlPersist = "10m";
    controlPath = "${config.home.homeDirectory}/.ssh/sockets/%u-%l-%r@%h:%p";
    matchBlocks = {
      "github.com" = {
        hostname = "github.com";
        user = "git";
        extraOptions = {
          controlMaster = "auto";
          controlPersist = "360";
        };
      };
      "gitlab.com" = {
        hostname = "gitlab.com";
        user = "git";
        extraOptions = {
          controlMaster = "auto";
          controlPersist = "360";
        };
      };
      "git.sr.ht" = {
        hostname = "git.sr.ht";
        user = "git";
        extraOptions = {
          controlMaster = "auto";
          controlPersist = "360";
        };
      };
      "*.redhat.com" = {
        user = "vdemeest";
      };
      "192.168.1.*" = {
        forwardAgent = true;
      };
      "10.100.0.*" = {
        forwardAgent = true;
      };
    } // homeConfig // vpnConfig;
    extraConfig = ''
      PreferredAuthentications gssapi-with-mic,publickey,password
      GSSAPIAuthentication yes
      GSSAPIDelegateCredentials yes
      StreamLocalBindUnlink yes
    '';
  };
}
