{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.modules.profiles.work;
in
{
  options = {
    modules.profiles.work = {
      redhat = mkEnableOption "Enable the Red Hat profiles (VPN, certs, …)";
    };
  };
  config = mkIf cfg.redhat {
    environment.systemPackages = with pkgs; [
      krb5
      (google-chrome.override {
        commandLineArgs = "--auth-negotiate-delegate-whitelist='*.redhat.com' --auth-server-whitelist=.redhat.com --enable-features=UseOzonePlatform --enable-gpu --ozone-platform=wayland";
      })
      libnotify
    ];
    # Kerberos
    age.secrets."krb5.conf" = {
      file = ../../../secrets/redhat/krb5.conf.age;
      path = "/etc/krb5.conf";
      mode = "444";
      group = "wheel";
    };
    # NetworkManager
    age.secrets."RHVPN.ovpn" = {
      file = ../../../secrets/redhat/RHVPN.ovpn.age;
      path = "/etc/NetworkManager/system-connections/RHVPN.ovpn";
      mode = "600";
    };
    age.secrets."redhat/AMS2.ovpn" = {
      file = ../../../secrets/redhat/AMS2.ovpn.age;
      path = "/etc/NetworkManager/system-connections/AMS2.ovpn";
      mode = "600";
    };
    age.secrets."BBRQ.ovpn" = {
      file = ../../../secrets/redhat/BBRQ.ovpn.age;
      path = "/etc/NetworkManager/system-connections/BBRQ.ovpn";
      mode = "600";
    };
    age.secrets."RDU2.ovpn" = {
      file = ../../../secrets/redhat/RDU2.ovpn.age;
      path = "/etc/NetworkManager/system-connections/RDU2.ovpn";
      mode = "600";
    };
    # Certificates
    age.secrets."ipa.crt" = {
      file = ../../../secrets/redhat/ipa.crt.age;
      path = "/etc/ipa/ipa.crt";
      mode = "444";
    };
    age.secrets."2015-RH-IT-Root-CA.pem" = {
      file = ../../../secrets/redhat/2015-RH-IT-Root-CA.pem.age;
      path = "/etc/pki/tls/certs/2015-RH-IT-Root-CA.pem";
      mode = "444";
    };
    age.secrets."Eng-CA.crt" = {
      file = ../../../secrets/redhat/Eng-CA.crt.age;
      path = "/etc/pki/tls/certs/Eng-CA.crt";
      mode = "444";
    };
    age.secrets."newca.crt" = {
      file = ../../../secrets/redhat/newca.crt.age;
      path = "/etc/pki/tls/certs/newca.crt";
      mode = "444";
    };
    age.secrets."oracle_ebs.crt" = {
      file = ../../../secrets/redhat/oracle_ebs.crt.age;
      path = "/etc/pki/tls/certs/oracle_ebs.crt";
      mode = "444";
    };
    age.secrets."pki-ca-chain.crt" = {
      file = ../../../secrets/redhat/pki-ca-chain.crt.age;
      path = "/etc/pki/tls/certs/pki-ca-chain.crt";
      mode = "444";
    };
    age.secrets."RH_ITW.crt" = {
      file = ../../../secrets/redhat/RH_ITW.crt.age;
      path = "/etc/pki/tls/certs/RH_ITW.crt";
      mode = "444";
    };
    age.secrets."win-intermediate-ca.cer" = {
      file = ../../../secrets/redhat/win-intermediate-ca.cer.age;
      path = "/etc/pki/tls/certs/win-intermediate-ca.cer";
      mode = "444";
    };
  };

}
