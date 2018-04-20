{ pkgs, prefix, ...}:

let home_directory = builtins.getEnv "HOME"; in

rec {
  programs = {
    home-manager = {
      enable = true;
      path = "${home_directory}/src/nix/home-manager";
    };
  };
  home.packages = with pkgs; [
    jq
    htop
    pass
    tree
  ];
}
