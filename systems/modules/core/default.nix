{ config, lib, pkgs, ... }:

{
  imports = [
    ./binfmt.nix
    ./boot.nix
    ./config.nix
    ./i18n.nix
    ./nix.nix
    ./users.nix
  ];

  environment.systemPackages = with pkgs; [
    cachix
    file
    htop
    iotop
    lsof
    netcat
    psmisc
    pv
    tree
    vim
    wget
  ];
  # FIXME fix tmpOnTmpfs
  # systemd.additionalUpstreamSystemUnits = [ "tmp.mount" ];

  security.sudo = {
    extraConfig = ''
      Defaults env_keep += SSH_AUTH_SOCK
    '';
  };

  age.secrets."minica.pem" = {
    file = ../../../secrets/minica.pem.age;
    path = "/etc/ssl/certs/minica.pem";
  };
  age.secrets."redhat.pem" = {
    file = ../../../secrets/redhat/redhat.pem.age;
    path = "/etc/ssl/certs/redhat.pem";
  };

  security.pki.certificates = [
    ''
          -----BEGIN CERTIFICATE-----
          MIIDSzCCAjOgAwIBAgIIJx9OWfxgsgswDQYJKoZIhvcNAQELBQAwIDEeMBwGA1UE
          AxMVbWluaWNhIHJvb3QgY2EgMjcxZjRlMCAXDTI0MDIyODA5MjIxNVoYDzIxMjQw
          MjI4MDkyMjE1WjAgMR4wHAYDVQQDExVtaW5pY2Egcm9vdCBjYSAyNzFmNGUwggEi
          MA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQC8UCyxEPiBME64NkwlOWLh19qE
          mlu3rAoT+CiFtEaUdjmEN34Emw/Tt0QbsKDhpkAEQwlsHuw5GFCI4ueI3bbC2LV1
          qkWVLgRrd6qduKnndzvDp8FwXHZFlG7tz0ds83cg4iRdPBt/jwHrqCm5sLVMaKKG
          heg+JlvEhGjiSlXQUIxUZGPpykVca1SHdleIWPb6fCLPogwo1mTMHPa8Flmze0IF
          bNhhlrW/eL5qoO96/tw/EsKGj6qzmjfGfCN76yu58tcW/bKaP18/DI6gLDw3c7b7
          oWlX8w+PBdOcmdllHiGdmHk/jojHPLy8kNAG/mSZC0j8w+pAI2IUhQO720QRAgMB
          AAGjgYYwgYMwDgYDVR0PAQH/BAQDAgKEMB0GA1UdJQQWMBQGCCsGAQUFBwMBBggr
          BgEFBQcDAjASBgNVHRMBAf8ECDAGAQH/AgEAMB0GA1UdDgQWBBRRiUxAxMcL9FFg
          MiJq1YVpsbXVijAfBgNVHSMEGDAWgBRRiUxAxMcL9FFgMiJq1YVpsbXVijANBgkq
          hkiG9w0BAQsFAAOCAQEARNZy1lSXCQwXKCrIPDnEu58QBLiaCFTyUtc1/IuHgpjy
          0Rvi5jnGj2pSJqG0lXEeHeMrs9dbFW7SJkC7HnCnER9Y3+Q712AG4vvagcD3lbrQ
          GfDSOa5tKtEESRlSfoQ1NEr5es4TRXxZrEL5yKU+GV6j3AtHFkgLYlcMbqZsbJIK
          Ag5Oy9aIjnXJ2cjRE+Y0Onsbmzp8sv7JefJ8KcZh23v6LBki7NCOLyetlpuikGGg
          58SnY+5uWgUV8coraFWPnmFC8Y+yHLmWCousU/t8FvEXIuYtc5PQm3RI9zG75Jp1
          cJBDKkh55goYNkjU4zjdljsEImjoBlJdP0hpiyQKAg==
          -----END CERTIFICATE-----
      		''
    ''
          -----BEGIN CERTIFICATE-----
          MIIENDCCAxygAwIBAgIJANunI0D662cnMA0GCSqGSIb3DQEBCwUAMIGlMQswCQYD
          VQQGEwJVUzEXMBUGA1UECAwOTm9ydGggQ2Fyb2xpbmExEDAOBgNVBAcMB1JhbGVp
          Z2gxFjAUBgNVBAoMDVJlZCBIYXQsIEluYy4xEzARBgNVBAsMClJlZCBIYXQgSVQx
          GzAZBgNVBAMMElJlZCBIYXQgSVQgUm9vdCBDQTEhMB8GCSqGSIb3DQEJARYSaW5m
          b3NlY0ByZWRoYXQuY29tMCAXDTE1MDcwNjE3MzgxMVoYDzIwNTUwNjI2MTczODEx
          WjCBpTELMAkGA1UEBhMCVVMxFzAVBgNVBAgMDk5vcnRoIENhcm9saW5hMRAwDgYD
          VQQHDAdSYWxlaWdoMRYwFAYDVQQKDA1SZWQgSGF0LCBJbmMuMRMwEQYDVQQLDApS
          ZWQgSGF0IElUMRswGQYDVQQDDBJSZWQgSGF0IElUIFJvb3QgQ0ExITAfBgkqhkiG
          9w0BCQEWEmluZm9zZWNAcmVkaGF0LmNvbTCCASIwDQYJKoZIhvcNAQEBBQADggEP
          ADCCAQoCggEBALQt9OJQh6GC5LT1g80qNh0u50BQ4sZ/yZ8aETxt+5lnPVX6MHKz
          bfwI6nO1aMG6j9bSw+6UUyPBHP796+FT/pTS+K0wsDV7c9XvHoxJBJJU38cdLkI2
          c/i7lDqTfTcfLL2nyUBd2fQDk1B0fxrskhGIIZ3ifP1Ps4ltTkv8hRSob3VtNqSo
          GxkKfvD2PKjTPxDPWYyruy9irLZioMffi3i/gCut0ZWtAyO3MVH5qWF/enKwgPES
          X9po+TdCvRB/RUObBaM761EcrLSM1GqHNueSfqnho3AjLQ6dBnPWlo638Zm1VebK
          BELyhkLWMSFkKwDmne0jQ02Y4g075vCKvCsCAwEAAaNjMGEwHQYDVR0OBBYEFH7R
          4yC+UehIIPeuL8Zqw3PzbgcZMB8GA1UdIwQYMBaAFH7R4yC+UehIIPeuL8Zqw3Pz
          bgcZMA8GA1UdEwEB/wQFMAMBAf8wDgYDVR0PAQH/BAQDAgGGMA0GCSqGSIb3DQEB
          CwUAA4IBAQBDNvD2Vm9sA5A9AlOJR8+en5Xz9hXcxJB5phxcZQ8jFoG04Vshvd0e
          LEnUrMcfFgIZ4njMKTQCM4ZFUPAieyLx4f52HuDopp3e5JyIMfW+KFcNIpKwCsak
          oSoKtIUOsUJK7qBVZxcrIyeQV2qcYOeZhtS5wBqIwOAhFwlCET7Ze58QHmS48slj
          S9K0JAcps2xdnGu0fkzhSQxY8GPQNFTlr6rYld5+ID/hHeS76gq0YG3q6RLWRkHf
          4eTkRjivAlExrFzKcljC4axKQlnOvVAzz+Gm32U0xPBF4ByePVxCJUHw1TsyTmel
          RxNEp7yHoXcwn+fXna+t5JWh1gxUZty3
          -----END CERTIFICATE-----
          -----BEGIN CERTIFICATE-----
          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
          56vmq8fwQYIcazjrygWiaOnoep/SMw==
          -----END CERTIFICATE-----
      		''
  ];

  security.pki.certificateFiles = [
    "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
    # "/etc/ssl/certs/minica.pem"
    # "/etc/ssl/certs/redhat.pem"
  ];

  # Only keep the last 500MiB of systemd journal.
  services.journald.extraConfig = "SystemMaxUse=500M";

  # Clear out /tmp after a fortnight and give all normal users a ~/tmp
  # cleaned out weekly.
  systemd.tmpfiles.rules = [ "d /tmp 1777 root root 14d" ] ++
    (
      let mkTmpDir = n: u: "d ${u.home}/tmp 0700 ${n} ${u.group} 7d";
      in lib.mapAttrsToList mkTmpDir (lib.filterAttrs (_: u: u.isNormalUser) config.users.extraUsers)
    );

  systemd.services."status-email-root@" = {
    description = "status email for %i to vincent";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = ''
        ${pkgs.my.systemd-email}/bin/systemd-email vincent@demeester.fr %i
      '';
      User = "root";
      Environment = "PATH=/run/current-system/sw/bin";
    };
  };
}
