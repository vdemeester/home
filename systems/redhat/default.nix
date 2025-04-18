{ config, ... }: {
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
}
