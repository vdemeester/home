{ pkgs, config, lib, ... }:

{
  home.file.".ssh/sockets/.placeholder".text = '''';
  programs.ssh = {
    enable = true;

    serverAliveInterval = 60;
    hashKnownHosts = true;
    userKnownHostsFile = "~/.config/ssh/known_hosts";
    controlPath = "~/.ssh/sockets/%u-%l-%r@%h:%p";
    
    matchBlocks = rec {
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
      "*.local" = {
        extraOptions = {
          controlMaster = "auto";
          controlPersist = "360";
        };
      };
    };
  };
}
