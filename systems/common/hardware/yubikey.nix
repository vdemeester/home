{ pkgs
, desktop
, ...
}:
{
  environment.systemPackages = with pkgs; [
    yubico-piv-tool
    yubikey-personalization
    yubikey-manager
  ] ++ lib.optional (builtins.isString desktop) [
    yubioath-flutter # Maybe not necessary
  ];

  programs.yubikey-touch-detector.enable = (builtins.isString desktop);

  services = {
    pcscd.enable = true;
    udev = {
      packages = [ pkgs.yubikey-personalization ];
      # FIXME: is it necessary ?
      extraRules = ''
        # Yubico YubiKey
        KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="1050", ATTRS{idProduct}=="0113|0114|0115|0116|0120|0402|0403|0406|0407|0410", TAG+="uaccess", MODE="0660", GROUP="wheel"
        # ACTION=="remove", ENV{ID_VENDOR_ID}=="1050", ENV{ID_MODEL_ID}=="0113|0114|0115|0116|0120|0402|0403|0406|0407|0410", RUN+="${pkgs.systemd}/bin/loginctl lock-sessions"
      '';
    };
  };

  security.pam.u2f = {
    enable = true;
    origin = "pam://yubi";
    authFile = pkgs.writeText "u2f-mappings" (lib.concatStrings [
      "vincent"
      ":4IiWZI9g6D8W6LeAW13ug4CnS8PreNRcHdcebkUDny3gWGfmpMJg4TgBWaZSIdh+sgg4jQA4MxYwTCmmP/ipWQ==,qOl+ouBRk6MMEJiE7H5LuTAirhBhN0UQrCNlLQoRsVttp6IBKG4yq4zDwm4fmYlfy1MFhvh7oOapMOmodMKJpQ==,es256,+presence" # yubikey5-a
      ":Sz4J2qMhoE7bE/uzwUzjJxG/bE0s+cw18zXcQjRsLIdJTVbuMad1ivKlYeLZW6vWV0lYiODlRW21HTSaFzu06A==,p7OZ3z5fiAIuJRHVzm56Y8Ti934+4cVHjsG7kaapmz8cWPfXfXfj5c8QiyIz3EQ0hOoxVV5cbkzUTxe7hdQIsA==,es256,+presence" # yubikey5-c1
    ]);
  };

  programs.gnupg.agent.pinentryPackage = pkgs.pinentry-gnome3;
  services.yubikey-agent.enable = true;
};
