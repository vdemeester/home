{ pkgs, ... }:

{
  imports = [ ./base.nix ];
  programs = {
    man.enable = false;
  };
  profiles.bash.enable = false;
  home.extraOutputsToInstall = [ "man" ];
}
