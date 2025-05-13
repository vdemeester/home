{
  lib,
  hardwareType,
  pkgs,
  ...
}:
let
  # Detect if we are building RPI4 host, because RPI4 doesn't have TPM support
  isRPI4 = hardwareType == "rpi4";
in
{
  environment.systemPackages =
    if isRPI4 then
      [ ]
    else
      with pkgs;
      [
        tpm2-tss
      ];
  security = lib.mkIf (!isRPI4) {
    tpm2 = {
      enable = true;
      pkcs11.enable = true;
      abrmd.enable = true;
    };
  };
}
