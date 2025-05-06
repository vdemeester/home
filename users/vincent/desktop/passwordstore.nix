{ config, pkgs, ... }:

{
  home.sessionVariables = {
    "PASSAGE_DIR" = "${config.xdg.dataHome}/passage";
    "PASSAGE_IDENTITIES_FILE" = "${config.xdg.dataHome}/passage/identities";
  };
  # TODO Migrate to passage
  programs.password-store = {
    enable = true;
    package = pkgs.pass-wayland.withExtensions (exts: [
      exts.pass-otp
      exts.pass-genphrase
      exts.pass-update
    ]);
  };
  home.packages = with pkgs; [
    wofi-pass
    passage
  ];
}
