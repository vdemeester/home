{ pkgs, ...}:

{
  imports = [ ../modules/module-list.nix ];
  programs = {
    home-manager = {
      enable = true;
      path = https://github.com/rycee/home-manager/archive/master.tar.gz;
    };
  };
  home.file.".nix-channels".source = ../assets/nix-channels;
  home.packages = with pkgs; [
    direnv
    enchive
    entr
    envsubst
    exa
    fd
    htop
    scripts
    tree
  ];
}
