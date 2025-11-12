{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    krb5
    (google-chrome.override {
      commandLineArgs = "--auth-negotiate-delegate-whitelist='*.redhat.com' --auth-server-whitelist=.redhat.com --enable-features=UseOzonePlatform --enable-gpu --ozone-platform=wayland";
    })
    # FIXME split this into real things
    oath-toolkit
  ];
  # Kerberos
  age.secrets."krb5.conf" = {
    file = ../../secrets/redhat/krb5.conf.age;
    path = "/etc/krb5.conf";
    mode = "444";
    group = "wheel";
  };
  # NetworkManager
  age.secrets."RHVPN.ovpn" = {
    file = ../../secrets/redhat/RHVPN.ovpn.age;
    path = "/etc/NetworkManager/system-connections/RHVPN.ovpn";
    mode = "600";
  };
  age.secrets."redhat/AMS2.ovpn" = {
    file = ../../secrets/redhat/AMS2.ovpn.age;
    path = "/etc/NetworkManager/system-connections/AMS2.ovpn";
    mode = "600";
  };
  age.secrets."BBRQ.ovpn" = {
    file = ../../secrets/redhat/BBRQ.ovpn.age;
    path = "/etc/NetworkManager/system-connections/BBRQ.ovpn";
    mode = "600";
  };
  age.secrets."RDU2.ovpn" = {
    file = ../../secrets/redhat/RDU2.ovpn.age;
    path = "/etc/NetworkManager/system-connections/RDU2.ovpn";
    mode = "600";
  };
  # Certificates
  age.secrets."ipa.crt" = {
    file = ../../secrets/redhat/ipa.crt.age;
    path = "/etc/ipa/ipa.crt";
    mode = "444";
  };
  age.secrets."2022-RH-IT-Root-CA.pem" = {
    file = ../../secrets/redhat/2022-RH-IT-Root-CA.pem.age;
    path = "/etc/pki/tls/certs/2022-RH-IT-Root-CA.pem";
    mode = "444";
  };

  # security.pki.certificates =[];
  security.pki.certificateFiles = [
    "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
    # "${config.age.secrets."2022-RH-IT-Root-CA.pem".path}"
    # "/home/vincent/tmp/2022-IT-Root-CA.pem"
    ../../secrets/redhat/2022-IT-Root-CA.pem
  ];
}
