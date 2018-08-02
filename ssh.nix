{ pkgs, config, lib, ... }:

{
  programs.ssh = {
    enable = true;

    controlMaster = "auto";
    controlPath = "/tmp/ssh-%u-%l-%r@%h:%p";
    controlPersist = "360";

    forwardAgent = true;
    serverAliveInterval = 60;
    
    hashKnownHosts = true;
    userKnownHostsFile = "~/.config/ssh/known_hosts";

    matchBlocks = rec {
      hokkaido-remote = {
        proxyCommand = "${pkgs.openssh}/bin/ssh -q p.sbr.pm nc localhost 2223";
        user = "vincent";
      };
      honshu-remote = {
        proxyCommand = "${pkgs.openssh}/bin/ssh -q p.sbr.pm nc localhost 2224";
        user = "vincent";
      };
    };
  };
}
