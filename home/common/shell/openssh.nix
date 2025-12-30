{
  pkgs,
  config,
  globals,
  libx,
  ...
}:
{
  home.packages = with pkgs; [
    sshfs
  ];
  # services.ssh-agent.enable = true;
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks = {
      "*" = {
        serverAliveInterval = 60;
        hashKnownHosts = true;
        userKnownHostsFile = "${config.home.homeDirectory}/.ssh/known_hosts";
        addKeysToAgent = "confirm";
        controlMaster = "auto";
        controlPersist = "10m";
        controlPath = "${config.home.homeDirectory}/.ssh/master-%C";
      };
      # Shpool session aliases (https://bower.sh/you-might-not-need-tmux)
      # Usage: ssh <host>/<session-name>
      # Example: ssh rhea.home/music, ssh aomi.home/dev
    }
    // (
      # Generate shpool session aliases for each machine dynamically
      let
        lib = pkgs.lib;
        mkShpoolAliases =
          _: machine:
          let
            # Get hostname identifiers (e.g., "rhea.home", "rhea.vpn", "rhea.sbr.pm")
            identifiers = builtins.filter (
              x: (lib.hasSuffix ".home" x) || (lib.hasSuffix ".vpn" x) || (lib.hasSuffix ".sbr.pm" x)
            ) (libx.sshHostIdentifier machine);
            # For each identifier, create a Host block with /* wildcard
            mkSessionBlock = id: {
              name = "${id}/*";
              value = {
                hostname =
                  if (lib.hasSuffix ".vpn" id) then
                    builtins.head machine.net.vpn.ips
                  else if (lib.hasSuffix ".home" id) then
                    builtins.head machine.net.ips
                  else
                    id;
                extraOptions = {
                  RemoteCommand = "shpool attach -f $(echo '%k' | cut -d/ -f2-)";
                  RequestTTY = "yes";
                };
              };
            };
          in
          builtins.listToAttrs (map mkSessionBlock identifiers);
      in
      # Merge all shpool aliases for all machines
      lib.attrsets.mergeAttrsList (lib.attrsets.mapAttrsToList mkShpoolAliases globals.machines)
    )
    // {
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
      "bootstrap.ospqa.com" = {
        forwardAgent = true;
      };
      "192.168.1.*" = {
        forwardAgent = true;
        extraOptions = {
          StrictHostKeyChecking = "no";
          UserKnownHostsFile = "/dev/null";
          identityFile = "~/.ssh/kyushu";
          # identityAgent = "empty";
        };
      };
      "10.100.0.*" = {
        forwardAgent = true;
        identityFile = "~/.ssh/kyushu";
        # identityAgent = "empty";
      };
    }
    // libx.sshConfigs globals.machines;
    extraConfig = ''
      # IdentityAgent /run/user/1000/yubikey-agent/yubikey-agent.sock
      GlobalKnownHostsFile ~/.ssh/ssh_known_hosts ~/.ssh/ssh_known_hosts.redhat ~/.ssh/ssh_known_hosts.mutable
      StrictHostKeyChecking yes
      PreferredAuthentications publickey,password
      StreamLocalBindUnlink yes
      IdentityFile ~/.ssh/keys/%h
      IdentityFile ~/.ssh/id_ed25519
    '';
  };
  home.file.".ssh/ssh_known_hosts".text = libx.sshKnownHosts globals.machines;
  home.file.".ssh/ssh_known_hosts.redhat".text = ''
    # Red Hat
    gitlab.cee.redhat.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICBgflBIyju1LV/29PmFDw0GLdB9h0JUXglNrvWjBQ2u
    code.engineering.redhat.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINYZZXmzm14TUL02Qe5SCMw48OfrphoIzi4qXSEK9Hiq
  '';
}
