{
  lib,
  hardwareType,
  pkgs,
  system,
  ...
}:
let
  # Systems without TPM hardware
  # - rpi4: Raspberry Pi 4
  # - Most aarch64 SBCs (Radxa CM3588, etc.) don't have TPM chips
  # For aarch64, only enable TPM if explicitly set via hardwareType
  hasNoTPM = hardwareType == "rpi4" || (system == "aarch64-linux" && hardwareType == "");
in
{
  environment.systemPackages =
    if hasNoTPM then
      [ ]
    else
      with pkgs;
      [
        tpm2-tss
      ];
  security = lib.mkIf (!hasNoTPM) {
    tpm2 = {
      enable = true;
      pkcs11.enable = true;
      abrmd.enable = true;
    };
  };
}
