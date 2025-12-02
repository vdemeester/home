{
  pkgs,
  config,
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
    # controlMaster = "auto";
    # controlPersist = "10m"; # FIXME: issue with OpenSSH 10p1, can re-enable in a few weeks.
    # controlPath = "${config.home.homeDirectory}/.ssh/master-%C";
    matchBlocks = {
      "*" = {
        serverAliveInterval = 60;
        hashKnownHosts = true;
        userKnownHostsFile = "${config.home.homeDirectory}/.ssh/known_hosts";
        addKeysToAgent = "confirm";
      };
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
    // (if config ? osConfig then libx.sshConfigs config.osConfig.infrastructure.machines else { });
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
  home.file.".ssh/ssh_known_hosts".text =
    if config ? osConfig then libx.sshKnownHosts config.osConfig.infrastructure.machines else "";
  home.file.".ssh/ssh_known_hosts.redhat".text = ''
    # Red Hat
    gitlab.cee.redhat.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICBgflBIyju1LV/29PmFDw0GLdB9h0JUXglNrvWjBQ2u
    code.engineering.redhat.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINYZZXmzm14TUL02Qe5SCMw48OfrphoIzi4qXSEK9Hiq
  '';
}
