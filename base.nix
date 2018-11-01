{ pkgs, prefix, ...}:

let home_directory = builtins.getEnv "HOME"; in

rec {
  imports = [ modules/module-list.nix ];
  programs = {
    home-manager = {
      enable = true;
      path = https://github.com/rycee/home-manager/archive/master.tar.gz;
    };
  };
  home.file.".nix-channels".source = ./nix-channels;
  home.packages = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.fr
    direnv
    enchive
    entr
    exa
    fd
    htop
    jq #dev
    pass
    scripts
    tree
    yq #dev
  ];
}
