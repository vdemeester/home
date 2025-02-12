let
  vincent-yubikey5a = "";
  #vincent-yubikey5a = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBFT5Rx+4Wuvd8lMBkcHxb4oHdRhm/OTg+p5tvPzoIN9enSmgRw5Inm/SlS8ZzV87G1NESTgzDRi6hREvqDlKvxs=";
  vincent-yubikey5c1 = "age1yubikey1q0g72w5n3zgt4qv64fkymcttqlpct0yh0rf29079h3696d6wkruakkst877"; # does this work ? Otherwise the ssh one.
  # vincent-yubikey5c1 = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBGHMa4rHuBbQQYv+8jvlkFCD2VYRGA4+5fnZAhLx8iDirzfEPqHB60UJWcDeixnJCUlpJjzFbS4crNOXhfCTCTE=";
  # vincent-yubikey5c2 = "";
  users = [ vincent-yubikey5c1 vincent-yubikey5a ];

  aomi = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFQVlSrUKU0xlM9E+sJ8qgdgqCW6ePctEBD2Yf+OnyME"; # ssh-keyscan -q -t ed25519 aomi.sbr.pm
  athena = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM/4KRP1rzOwyA2zP1Nf1WlLRHqAGutLtOHYWfH732xh"; # ssh-keyscan -q -t ed25519 athena.sbr.pm
  demeter = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGqQfEyHyjIGglayB9FtCqL7bnYfNSQlBXks2IuyCPmd"; # ssh-keyscan -q -t ed25519 demeter.sbr.pm
  kerkouane = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJguVoQYObRLyNxELFc3ai2yDJ25+naiM3tKrBGuxwwA"; # ssh-keyscan -q -t ed25519 kerkouane.sbr.pm
  sakhalin = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN/PMBThi4DhgZR8VywbRDzzMVh2Qp3T6NJAcPubfXz6"; # ssh-keyscan -q -t ed25519 sakhalin.sbr.pm
  shikoku = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH18c6kcorVbK2TwCgdewL6nQf29Cd5BVTeq8nRYUigm"; # ssh-keyscan -q -t ed25519 shikoku.sbr.pm
  wakasu = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINrAh07USjRnAdS3mMNGdKee1KumjYDLzgXaiZ5LYi2D"; # ssh-keyscan -q -t ed25519 wakasu.sbr.pm
  desktops = [ aomi wakasu ];
  servers = [ athena demeter kerkouane sakhalin shikoku ];
  systems = servers ++ desktops;
in
{
  # Red Hat
  "secrets/redhat/krb5.conf.age".publicKeys = users ++ [ aomi wakasu ];
  "secrets/redhat/RHVPN.ovpn.age".publicKeys = users ++ [ aomi wakasu ];
  "secrets/redhat/AMS2.ovpn.age".publicKeys = users ++ [ aomi wakasu ];
  "secrets/redhat/RDU2.ovpn.age".publicKeys = users ++ [ aomi wakasu ];
  "secrets/redhat/BBRQ.ovpn.age".publicKeys = users ++ [ aomi wakasu ];
  "secrets/redhat/ipa.crt.age".publicKeys = users ++ [ aomi wakasu ];
  "secrets/redhat/2015-RH-IT-Root-CA.pem.age".publicKeys = users ++ [ aomi wakasu ];
  "secrets/redhat/Eng-CA.crt.age".publicKeys = users ++ [ aomi wakasu ];
  "secrets/redhat/newca.crt.age".publicKeys = users ++ [ aomi wakasu ];
  "secrets/redhat/oracle_ebs.crt.age".publicKeys = users ++ [ aomi wakasu ];
  "secrets/redhat/pki-ca-chain.crt.age".publicKeys = users ++ [ aomi wakasu ];
  "secrets/redhat/RH_ITW.crt.age".publicKeys = users ++ [ aomi wakasu ];
  "secrets/redhat/win-intermediate-ca.cer.age".publicKeys = users ++ [ aomi wakasu ];
  "secrets/redhat/redhat.pem.age".publicKeys = users ++ systems;
  # Others
  "secrets/minica.pem.age".publicKeys = users ++ systems;
  "secrets/shikoku/aria2rpcsecret.age".publicKeys = users ++ [ shikoku ];
}
