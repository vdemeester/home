{
  config,
  pkgs,
  ...
}:
{
  environment.systemPackages =
    with pkgs;
    [
      age
      agenix
      passage
    ]
    ++ lib.optional config.security.tpm2.enable pkgs.age-plugin-tpm;
}
