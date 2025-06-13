let
  gpgRemoteForward = {
    bind.address = "/run/user/1000/gnupg/S.gpg-agent";
    host.address = "/run/user/1000/gnupg/S.gpg-agent.extra";
  };
  gpgSSHRemoteForward = {
    bind.address = "/run/user/1000/gnupg/S.gpg-agent.ssh";
    host.address = "/run/user/1000/gnupg/S.gpg-agent.ssh";
  };
  home = {
    ips = {
      aomi = "192.168.1.23";
      aion = "192.168.1.49";
      dev = "192.168.1.60";
      hokkaido = "192.168.1.115";
      honshu = "192.168.1.17";
      naruhodo = "192.168.1.36";
      okinawa = "192.168.1.19";
      sakhalin = "192.168.1.70";
      shikoku = "192.168.1.24";
      synodine = "192.168.1.20";
      kobe = "192.168.1.77";
      hass = "192.168.1.181";
      demeter = "192.168.1.182";
      athena = "192.168.1.183";
      remarkable = "192.168.1.57";
    };
  };
  wireguard = {
    ips = {
      kerkouane = "10.100.0.1";
      shikoku = "10.100.0.2";
      aomi = "10.100.0.17";
      kyushu = "10.100.0.19";
      hokkaido = "10.100.0.5";
      wakasu = "10.100.0.8";
      ipad = "10.100.0.3";
      vincent = "10.100.0.9";
      honshu = "10.100.0.10";
      houbeb = "10.100.0.13";
      houbeb-ipad = "10.100.0.43";
      houbeb-iphone = "10.100.0.44";
      okinawa = "10.100.0.14";
      naruhodo = "10.100.0.15";
      sakhalin = "10.100.0.16";
      hass = "10.100.0.81";
      demeter = "10.100.0.82";
      athena = "10.100.0.83";
      aix = "10.100.0.89";
      aion = "10.100.0.49";
      kobe = "10.100.0.77";
    };
    kerkouane = {
      allowedIPs = [ "${wireguard.ips.kerkouane}/32" ];
      publicKey = "+H3fxErP9HoFUrPgU19ra9+GDLQw+VwvLWx3lMct7QI=";
    };
    shikoku = {
      allowedIPs = [ "${wireguard.ips.shikoku}/32" ];
      publicKey = "foUoAvJXGyFV4pfEE6ISwivAgXpmYmHwpGq6X+HN+yA=";
    };
    wakasu = {
      allowedIPs = [ "${wireguard.ips.wakasu}/32" ];
      publicKey = "qyxGnd/YJefqb4eEPqKO5XinvNx14fPcuZMNeYuBvSQ=";
    };
    athena = {
      allowedIPs = [ "${wireguard.ips.athena}/32" ];
      publicKey = "RWqH7RdIXg+YE9U1nlsNiOC7jH8eWjWQmikqBVDGSXU=";
    };
    demeter = {
      allowedIPs = [ "${wireguard.ips.demeter}/32" ];
      publicKey = "/bBh4gvDty/AA2qIiHc7K0OHoOXWmj2SFFXdDq8nsUU=";
    };
    aix = {
      allowedIPs = [ "${wireguard.ips.aix}/32" ];
      publicKey = "D1HoBqrqBchHOOi8mjKpVg5vZtt+iFy8wj4o3kGYwkc=";
    };
    aion = {
      allowedIPs = [ "${wireguard.ips.aion}/32" ];
      publicKey = "T8qfsBiOcZNxUeRHFg+2FPdGj4AuGloJ4b+0uI2jM2w=";
    };
    vincent = {
      allowedIPs = [ "${wireguard.ips.vincent}/32" ];
      publicKey = "1wzFG60hlrAoSYcRKApsH+WK3Zyz8IjdLgIb/8JbuW0=";
    };
    ipad = {
      allowedIPs = [ "${wireguard.ips.ipad}/32" ];
      publicKey = "6viS+HqkW+qSj4X+Sj8n1PCJ6QIaZsOkmFQytlRvRwk=";
    };
    houbeb = {
      allowedIPs = [ "${wireguard.ips.houbeb}/32" ];
      publicKey = "RANYR3ouf+3Bh+6kJVF1I0s5iglBGs7ucW6iRyv2cGc=";
    };
    houbeb-ipad = {
      allowedIPs = [ "${wireguard.ips.houbeb-ipad}/32" ];
      publicKey = "cwdpRxMJP86QYkJ1O43mBoSyBJaE7d7Kxbsd2nj5vh4=";
    };
    houbeb-iphone = {
      allowedIPs = [ "${wireguard.ips.houbeb-iphone}/32" ];
      publicKey = "UdgP0UBPfIrzdUe8JNeom4qahtoBxSADI5gKwpoNYEs=";
    };
    okinawa = {
      allowedIPs = [ "${wireguard.ips.okinawa}/32" ];
      publicKey = "gsX8RiTq7LkCiEIyNk2j9b8CHlJjSUbi1Im6nSWGmB4=";
    };
    sakhalin = {
      allowedIPs = [ "${wireguard.ips.sakhalin}/32" ];
      publicKey = "OAjw1l0z56F8kj++tqoasNHEMIWBEwis6iaWNAh1jlk=";
    };
    aomi = {
      allowedIPs = [ "${wireguard.ips.aomi}/32" ];
      publicKey = "XT4D9YLeVHwMb9R4mhBLSWHYF8iBO/UOT86MQL1jnA4=";
    };
    kyushu = {
      allowedIPs = [ "${wireguard.ips.kyushu}/32" ];
      publicKey = "KVRzoPUw8UTQblYtbs/NLYLIVmtQehrc4Hacbpf5Ugs=";
    };
    kobe = {
      allowedIPs = [ "${wireguard.ips.kobe}/32" ];
      publicKey = "B9jLGtXGZEfvBrgyEKrFRrsCsTsarfpFeyXqqq1NOWg=";
    };
    hass = {
      allowedIPs = [ "${wireguard.ips.hass}/32" ];
      publicKey = "sLi6Qpm6yyI0kuJ5LzCKXzFhhTW3Q50krxSin+b/sWs=";
    };
    honshu = {
      allowedIPs = [ "${wireguard.ips.honshu}/32" ];
      publicKey = "vzkizZZMoM8yi5bjKiDZN1vYydZY6NKJhv9OqwSTFAk=";
    };
  };
  ssh = {
    # gpg deprecated
    yubikey = {
      key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCDr3bRw6r43BVOltmTXtDQAtZlJ/viBrCb58fG8suSdO97xLEGukZzf1QX46aXQEsenfKOalcd+OrukcoVIiZtlh1BHAaBB09Q0vKjtB1zKcUdZQYb6kA21/ItpW3gNsZq5M98QpwS9soJOLSccQosDoVBWDcHx72Kpzp2x4seKyAIpb1gtPnQjnnwA7urTcANw7CU8lmB3UtJZNPHclJNKso7h0ZBapausk9t0xGP18rmzQAe2ipa6pwUzS5rRq+j0LiY/JZQaQWBfc1i3IcKictKW5EykKmywJcwmr/PcTdcgTT4FaD+b1t1QAPLV82HxGzOYQO+/WBptBdq7Ss5 openpgp:0x86ADD81F";
      authorized = true;
    };
    # gpg deprecated
    yubikey5 = {
      # key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJoArpBsTXr3m6q2QnA1vI1DSwmgdU0OAp7DUxcxl9CJfeZIEs/iAerk8jmHgJ2xCEF6SpzI0FWSQIXy8dKpF4wLJ0tCoq5LqQx3jEzy3NUBLfxK+/Baa1te4qG2YImlgnzmEEm5uZlCGZRY2L/U9+4Hwo1AgD69Zzin6QGh2pyTWpmZ/WyhwIfGgqsnlM9XlaVzlMHYfStDi+rUU6XEAfdSqo1SnWKDBHc3mDYGTVhfAlt2LucLKu7oI2MsSlSxva072BExctadtB3TGHbt8gRJZj8CdwgRNhT+hFfbsL6YDvQn6dhTSMuiD8sBEvVble0Nj4p+Q6ROCRIuMuhgh3 cardno:000610153832";
      key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJoArpBsTXr3m6q2QnA1vI1DSwmgdU0OAp7DUxcxl9CJfeZIEs/iAerk8jmHgJ2xCEF6SpzI0FWSQIXy8dKpF4wLJ0tCoq5LqQx3jEzy3NUBLfxK+/Baa1te4qG2YImlgnzmEEm5uZlCGZRY2L/U9+4Hwo1AgD69Zzin6QGh2pyTWpmZ/WyhwIfGgqsnlM9XlaVzlMHYfStDi+rUU6XEAfdSqo1SnWKDBHc3mDYGTVhfAlt2LucLKu7oI2MsSlSxva072BExctadtB3TGHbt8gRJZj8CdwgRNhT+hFfbsL6YDvQn6dhTSMuiD8sBEvVble0Nj4p+Q6ROCRIuMuhgh3 cardno:10 153 832";
      authorized = true;
    };
    yubikey5-a = {
      key = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBFT5Rx+4Wuvd8lMBkcHxb4oHdRhm/OTg+p5tvPzoIN9enSmgRw5Inm/SlS8ZzV87G1NESTgzDRi6hREvqDlKvxs=";
      authorized = true;
    };
    yubikey5-c1 = {
      key = "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBGHMa4rHuBbQQYv+8jvlkFCD2VYRGA4+5fnZAhLx8iDirzfEPqHB60UJWcDeixnJCUlpJjzFbS4crNOXhfCTCTE=";
      authorized = true;
    };
    # yubikey5-c2 = {
    #   key = "";
    #   authorized = true;
    # };
    kerkouane = {
      port = 20000;
      key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILtEnw+3WMa9ESRyKdBUp/OHd8NPQdHLoqQ58L3YXF1o vincent@kerkouane";
      authorized = true;
    };
    california = {
      key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICl4uBPx98p0m1ra4nKxaDvCP8TCou5J10gFUpYAuzp9 u0_a103@localhost";
    };
    hokkaido = {
      key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKcmRh9Khviqrl9wPPzogW9vTMAtkFc0HfWQ5kgvOpCw vincent@hokkaido";
      authorized = true;
    };
    wakasu = {
      key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIITpgxTnebhBnFyjWiF1nPM7Wl7qF+ce3xy/FvA4ZVN+ vincent@wakasu";
      authorized = true;
    };
    vincent = {
      key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINsbGtpU/w7Ff3O7hJ1QoO/5CuCrssBXrT+iHev/+rbf Generated By Termius";
    };
    houbeb = {
      key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGUnBCTxRoIDhExcSaiirM5nf2PIcTMDUodYlGNvqfmD Generated By Termius";
    };
    phantom = {
      key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDm23WasboyoiYcaCyxb/DWXRwWXR183gHwOcWTGMKZaYy0WMAWkBUPJjD5s7tlib2D7GJIoBqoPRvNQbmUdxFle+CftY7aj7oP7s0FlbNzFmybTzcZ/3zkkkKAOw2USw3saQ4kd8IqyACo9TsfhajX8jsrrHl3dzyjqTDWlcJmETUGpdYbSA7E3WavzPF2x3/kFcA5cmoYgpcFpGgXAKvaG2IFONLv+vTDPtGVq+GiOwQSVR7TXpFmdhHEw9hnzHnsuffQMxANaQMvqPV8+H0jfF3H2WNqp8GULcGyudngkKioTAVvBiTiRJnVK7hg6SxpdlszqO0yMjN37NB2gPJz houbeb@phantom.local";
    };
    okinawa = {
      key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILcu4MmZNeBLE7HDjLc6T10tz6rerziQbsZN0LS+mAiq vincd@okinawa";
    };
    honshu = {
      key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAocnNHVCqloXfsvbOoMV0KYAdeon5NYrZX3bnWK+SAo vincent@honshu";
    };
    aomi = {
      key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHJ3QqVCUiE4BIFKTJLN6mSnp9bLSnJ3gE8ScbAajGsH vincent@aomi";
      authorized = true;
    };
  };
in
{
  inherit home;
  inherit wireguard;
  wg = {
    allowedIPs = "10.100.0.0/24";
    listenPort = 51820;
    endpointIP = "167.99.17.238";
    persistentKeepalive = 25;
    peers = [
      wireguard.shikoku
      wireguard.wakasu
      wireguard.vincent
      wireguard.sakhalin
      wireguard.aomi
      wireguard.kyushu
      wireguard.ipad
      wireguard.hass
      wireguard.demeter
      wireguard.athena
      wireguard.aix
      wireguard.aion
      wireguard.honshu
      wireguard.houbeb
      wireguard.houbeb-ipad
      wireguard.houbeb-iphone
    ]; # wireguard.hokkaido
  };
  inherit ssh;
  sshConfig = {
    "naruhodo.home" = {
      hostname = "${home.ips.naruhodo}";
    };
    "naruhodo.vpn" = {
      hostname = "${wireguard.ips.naruhodo}";
    };
    "aomi.home" = {
      hostname = "${home.ips.aomi}";
      remoteForwards = [
        gpgRemoteForward
        gpgSSHRemoteForward
      ];
    };
    "aion.home" = {
      hostname = "${home.ips.aion}";
      remoteForwards = [
        gpgRemoteForward
        gpgSSHRemoteForward
      ];
    };
    "aion.vpn" = {
      hostname = "${wireguard.ips.aion}";
      remoteForwards = [
        gpgRemoteForward
        gpgSSHRemoteForward
      ];
    };
    "aomi.vpn" = {
      hostname = "${wireguard.ips.aomi}";
      remoteForwards = [
        gpgRemoteForward
        gpgSSHRemoteForward
      ];
    };
    "okinawa.home" = {
      hostname = "${home.ips.okinawa}";
      remoteForwards = [
        gpgRemoteForward
        gpgSSHRemoteForward
      ];
    };
    "okinawa.vpn" = {
      hostname = "${wireguard.ips.okinawa}";
      remoteForwards = [
        gpgRemoteForward
        gpgSSHRemoteForward
      ];
    };
    "sakhalin.home" = {
      hostname = "${home.ips.sakhalin}";
      remoteForwards = [
        gpgRemoteForward
        gpgSSHRemoteForward
      ];
    };
    "sakhalin.vpn" = {
      hostname = "${wireguard.ips.sakhalin}";
      remoteForwards = [
        gpgRemoteForward
        gpgSSHRemoteForward
      ];
    };
    "hokkaido.home" = {
      hostname = "${home.ips.hokkaido}";
      remoteForwards = [
        gpgRemoteForward
        gpgSSHRemoteForward
      ];
    };
    "hokkaido.vpn" = {
      hostname = "${wireguard.ips.hokkaido}";
      remoteForwards = [
        gpgRemoteForward
        gpgSSHRemoteForward
      ];
    };
    "wakasu.home" = {
      hostname = "${home.ips.wakasu}";
      remoteForwards = [
        gpgRemoteForward
        gpgSSHRemoteForward
      ];
    };
    "wakasu.vpn" = {
      hostname = "${wireguard.ips.wakasu}";
      remoteForwards = [
        gpgRemoteForward
        gpgSSHRemoteForward
      ];
    };
    "athena.home" = {
      hostname = "${home.ips.athena}";
      remoteForwards = [
        gpgRemoteForward
        gpgSSHRemoteForward
      ];
    };
    "athena.vpn" = {
      hostname = "${wireguard.ips.athena}";
      remoteForwards = [
        gpgRemoteForward
        gpgSSHRemoteForward
      ];
    };
    "demeter.home" = {
      hostname = "${home.ips.demeter}";
      remoteForwards = [
        gpgRemoteForward
        gpgSSHRemoteForward
      ];
    };
    "demeter.vpn" = {
      hostname = "${wireguard.ips.demeter}";
      remoteForwards = [
        gpgRemoteForward
        gpgSSHRemoteForward
      ];
    };
    "dev.home" = {
      hostname = "${home.ips.dev}";
    };
    "kerkouane.vpn" = {
      hostname = "${wireguard.ips.kerkouane}";
      remoteForwards = [
        gpgRemoteForward
        gpgSSHRemoteForward
      ];
    };
  };
}
