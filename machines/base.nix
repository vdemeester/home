{ pkgs, prefix, ...}:

let home_directory = builtins.getEnv "HOME"; in

rec {
  imports = [ ../modules/module-list.nix ];
  programs = {
    home-manager = {
      enable = true;
      path = https://github.com/rycee/home-manager/archive/master.tar.gz;
    };
  };
  home.file.".nix-channels".source = ../assets/nix-channels;
  home.file.".msmtprc".source = /etc/msmtprc;
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
