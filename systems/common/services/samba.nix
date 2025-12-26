{
  pkgs,
  ...
}:
{
  services = {
    # Network shares
    samba = {
      package = pkgs.samba4Full;
      enable = true;
      openFirewall = true;
      settings = {
        global = {
          security = "user";
          workgroup = "WORKGROUP";
          "server smb encrypt" = "desired";
          "server signing" = "auto";
          "server min protocol" = "SMB3_00";
          "hosts allow" = "192.168. 10.100. 127.0.0.1 localhost";
          "hosts deny" = "0.0.0.0/0";
        };
      };
    };
    avahi = {
      publish.enable = true;
      publish.userServices = true;
      nssmdns4 = true;
      enable = true;
      openFirewall = true;
    };
    samba-wsdd = {
      enable = true;
      openFirewall = true;
    };
  };
}
