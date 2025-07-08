_: {
  services = {
    openssh = {
      enable = true;
      openFirewall = true;
      settings = {
        # FIXME: enable this
        # PasswordAuthentication = false;
        # PermitRootLogin = "no"
      };
      extraConfig = ''
        StreamLocalBindUnlink yes
      '';
    };
    sshguard.enable = true;
  };
  security.pam.sshAgentAuth.enable = true;
}
