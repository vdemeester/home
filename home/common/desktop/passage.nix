{ config, pkgs, ... }:
{
  home.sessionVariables = {
    "PASSAGE_DIR" = "${config.xdg.dataHome}/passage";
    "PASSAGE_IDENTITIES_FILE" = "${config.xdg.dataHome}/passage/identities";
  };

  home.packages = with pkgs; [
    passage
  ];
}
