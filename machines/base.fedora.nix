{ pkgs, ...}:

{
  imports = [ ../modules/module-list.nix ];
  programs = {
    home-manager = {
      enable = true;
    };
    man.enable = false;
  };
  profiles.bash.enable = false;
  home.extraOutputsToInstall = [ "man" ];
  home.file.".nix-channels".source = ../assets/nix-channels;
  home.packages = with pkgs; [
    direnv
    enchive
    entr
    exa
    fd
    htop
    scripts
    tree
  ];
}
