{ config, inputs, lib, pkgs, ... }:
let
  inherit (lib) mkIf;
in
{
  users.users.root = {
    shell = mkIf config.programs.zsh.enable pkgs.zsh;
  };
  # Home-manager "magic"
  home-manager.users.root = inputs.self.internal.homeManagerConfigurations."root";
}
