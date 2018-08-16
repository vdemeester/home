{ config, pkgs, ... }:

{
  users = {
    extraUsers = {
      vincent = {
        isNormalUser = true;
        uid = 1000;
        createHome = true;
        extraGroups = [ "networkmanager" "wheel" "docker" "vboxusers" "libvirtd" "input" "audio" "video" "scanner" ];
        shell = if config.programs.fish.enable then pkgs.fish else pkgs.bash;
        initialPassword = "changeMe";
        openssh.authorizedKeys.keys =
          with import ../ssh-keys.nix; [ honshu wakasu hokkaido kobe ];
      };
    };
  };
}
