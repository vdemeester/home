{ config, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption mkIf mkMerge mkOption types;
  cfg = config.modules.hardware.yubikey;
in
{
  options = {
    modules.hardware.yubikey = {
      enable = mkEnableOption "Enable yubikey profile";
      agent = mkOption {
        default = true;
        description = "wether to enable yubikey-agent";
        type = types.bool;
      };
      u2f = mkOption {
        default = true;
        description = "wether to enable auth with yubkeys throguh pam using u2f";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      environment = {
        systemPackages = with pkgs; [
          yubico-piv-tool
          yubikey-personalization
          yubikey-manager
        ];
      };
      services = {
        pcscd.enable = true;
        udev = {
          packages = with pkgs; [ yubikey-personalization ];
          extraRules = ''
            # Yubico YubiKey
            KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="1050", ATTRS{idProduct}=="0113|0114|0115|0116|0120|0402|0403|0406|0407|0410", TAG+="uaccess", MODE="0660", GROUP="wheel"
            # ACTION=="remove", ENV{ID_VENDOR_ID}=="1050", ENV{ID_MODEL_ID}=="0113|0114|0115|0116|0120|0402|0403|0406|0407|0410", RUN+="${pkgs.systemd}/bin/loginctl lock-sessions"
          '';
        };
      };
    }
    (mkIf config.modules.desktop.enable {
      environment.systemPackages = with pkgs; [
        yubioath-flutter
      ];
    })
    (mkIf cfg.u2f {
      security.pam.u2f = {
        enable = true;
        origin = "pam://yubi";
        authFile = pkgs.writeText "u2f-mappings" (lib.concatStrings [
          "vincent"
          ":4IiWZI9g6D8W6LeAW13ug4CnS8PreNRcHdcebkUDny3gWGfmpMJg4TgBWaZSIdh+sgg4jQA4MxYwTCmmP/ipWQ==,qOl+ouBRk6MMEJiE7H5LuTAirhBhN0UQrCNlLQoRsVttp6IBKG4yq4zDwm4fmYlfy1MFhvh7oOapMOmodMKJpQ==,es256,+presence" # yubikey5-a
          ":Sz4J2qMhoE7bE/uzwUzjJxG/bE0s+cw18zXcQjRsLIdJTVbuMad1ivKlYeLZW6vWV0lYiODlRW21HTSaFzu06A==,p7OZ3z5fiAIuJRHVzm56Y8Ti934+4cVHjsG7kaapmz8cWPfXfXfj5c8QiyIz3EQ0hOoxVV5cbkzUTxe7hdQIsA==,es256,+presence" # yubikey5-c1
        ]);
      };
    })
    (mkIf cfg.agent {
      services.yubikey-agent.enable = true;
    })
  ]);
}
