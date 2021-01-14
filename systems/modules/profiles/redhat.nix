{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.redhat;
in
{
  options = {
    profiles.redhat = {
      enable = mkEnableOption "Enable the Red Hat profiles (VPN, certs, …)";
    };
  };
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      krb5
      (google-chrome.override {
        commandLineArgs = "--auth-negotiate-delegate-whitelist='*.redhat.com'";
      })
      libnotify
    ];
    environment.etc."krb5.conf" = {
      source = pkgs.mkSecret ../../../secrets/etc/krb5.conf;
    };
    # NetworkManager
    environment.etc."NetworkManager/system-connections/1-RHVPN.ovpn" = {
      source = pkgs.mkSecret ../../../secrets/etc/NetworkManager/system-connections/1-RHVPN.ovpn;
      mode = "0600";
    };
    environment.etc."NetworkManager/system-connections/AMS2.ovpn" = {
      source = pkgs.mkSecret ../../../secrets/etc/NetworkManager/system-connections/AMS2.ovpn;
      mode = "0600";
    };
    environment.etc."NetworkManager/system-connections/BBRQ.ovpn" = {
      source = pkgs.mkSecret ../../../secrets/etc/NetworkManager/system-connections/BRQ.ovpn;
      mode = "0600";
    };
    environment.etc."NetworkManager/system-connections/RDU2.ovpn" = {
      source = pkgs.mkSecret ../../../secrets/etc/NetworkManager/system-connections/RDU2.ovpn;
      mode = "0600";
    };
    environment.etc."NetworkManager/system-connections/PNQ2.ovpn" = {
      source = pkgs.mkSecret ../../../secrets/etc/NetworkManager/system-connections/PNQ2.ovpn;
      mode = "0600";
    };
    environment.etc."NetworkManager/system-connections/FAB.ovpn" = {
      source = pkgs.mkSecret ../../../secrets/etc/NetworkManager/system-connections/FAB.ovpn;
      mode = "0600";
    };
    # Certificates
    environment.etc."ipa/ipa.crt".source = pkgs.mkSecret ../../../secrets/etc/ipa/ipa.crt;
    environment.etc."pki/tls/certs/2015-RH-IT-Root-CA.pem".source = pkgs.mkSecret ../../../secrets/etc/pki/tls/certs/2015-RH-IT-Root-CA.pem;
    environment.etc."pki/tls/certs/Eng-CA.crt".source = pkgs.mkSecret ../../../secrets/etc/pki/tls/certs/Eng-CA.crt;
    environment.etc."pki/tls/certs/newca.crt".source = pkgs.mkSecret ../../../secrets/etc/pki/tls/certs/newca.crt;
    environment.etc."pki/tls/certs/oracle_ebs.crt".source = pkgs.mkSecret ../../../secrets/etc/pki/tls/certs/oracle_ebs.crt;
    environment.etc."pki/tls/certs/pki-ca-chain.crt".source = pkgs.mkSecret ../../../secrets/etc/pki/tls/certs/pki-ca-chain.crt;
    environment.etc."pki/tls/certs/RH_ITW.crt".source = pkgs.mkSecret ../../../secrets/etc/pki/tls/certs/RH_ITW.crt;
    environment.etc."pki/tls/certs/win-intermediate-ca.cer".source = pkgs.mkSecret ../../../secrets/etc/pki/tls/certs/win-intermediate-ca.cer;
  };
}
