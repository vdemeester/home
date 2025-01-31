{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.profiles.work;
  common = {
    sopsFile = ../../../secrets/desktops/redhat.yaml;
    mode = "444";
    owner = "root";
    group = "root";
  };
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
      path = "/etc/krb5.conf";
    };
    # NetworkManager
    age.secrets."RHVPN.ovpn" = {
      path = "/etc/NetworkManager/system-connections/RHVPN.ovpn";
      mode = "600";
    };
    age.secrets."redhat/AMS2.ovpn" = {
      file = ../../../secrets/redhat/AMS2.ovpn.age;
      path = "/etc/NetworkManager/system-connections/AMS2.ovpn";
      mode = "600";
    };
    age.secrets."BBRQ.ovpn" = {
      path = "/etc/NetworkManager/system-connections/BBRQ.ovpn";
      mode = "600";
    };
    age.secrets."RDU2.ovpn" = {
      path = "/etc/NetworkManager/system-connections/RDU2.ovpn";
      mode = "600";
    };
    # Certificates
    sops.secrets."ipa.crt" = {
      inherit (common) mode owner group sopsFile;
      path = "/etc/ipa/ipa.crt";
    };
    sops.secrets."2015-RH-IT-Root-CA.pem" = {
      inherit (common) mode owner group sopsFile;
      path = "/etc/pki/tls/certs/2015-RH-IT-Root-CA.pem";
    };
    sops.secrets."Eng-CA.crt" = {
      inherit (common) mode owner group sopsFile;
      path = "/etc/pki/tls/certs/Eng-CA.crt";
    };
    sops.secrets."newca.crt" = {
      inherit (common) mode owner group sopsFile;
      path = "/etc/pki/tls/certs/newca.crt";
    };
    sops.secrets."oracle_ebs.crt" = {
      inherit (common) mode owner group sopsFile;
      path = "/etc/pki/tls/certs/oracle_ebs.crt";
    };
    sops.secrets."pki-ca-chain.crt" = {
      inherit (common) mode owner group sopsFile;
      path = "/etc/pki/tls/certs/pki-ca-chain.crt";
    };
    sops.secrets."RH_ITW.crt" = {
      inherit (common) mode owner group sopsFile;
      path = "/etc/pki/tls/certs/RH_ITW.crt";
    };
    sops.secrets."win-intermediate-ca.cer" = {
      inherit (common) mode owner group sopsFile;
      path = "/etc/pki/tls/certs/win-intermediate-ca.cer";
    };
  };

}
