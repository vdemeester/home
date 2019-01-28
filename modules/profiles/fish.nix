{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.fish;
in
{
  options = {
    profiles.fish = {
      enable = mkOption {
        default = true;
        description = "Enable fish program and configurations";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    programs.fish = {
    enable = true;
    shellAliases = import ./aliases.shell.nix;
    shellInit = ''
      eval (${pkgs.direnv}/bin/direnv hook fish)
      # emacs ansi-term support
      if test -n "$EMACS"
        set -x TERM eterm-color
  
        # this function may be required
        function fish_title
          true
        end
      end
      '';
    };
    xdg.configFile."fish/conf.d/sudope.fish".source = ./assets/fish/sudope.fish;
    xdg.configFile."fish/functions/sudope.fish".source = ./assets/fish/sudope.function.fish;
    xdg.configFile."fish/functions/fish_prompt.fish".source = ./assets/fish/fish_prompt.fish;
    xdg.configFile."fish/functions/fish_right_prompt.fish".source = ./assets/fish/fish_right_prompt.fish;
    xdg.configFile."nr/default" = {
      text = builtins.toJSON [
        {cmd = "ncdu";} {cmd = "sshfs";} {cmd = "gotop";} {cmd = "pandoc";} {cmd = "dep2nix";} {cmd = "go2nix";}
        {cmd = "lspci"; pkg = "pciutils";}
        {cmd = "lsusb"; pkg = "usbutils";}
        {cmd = "wakeonlan"; pkg = "python36Packages.wakeonlan";}
        {cmd = "beet"; pkg = "beets";}
        {cmd = "virt-manager"; pkg = "virtmanager";}
        {cmd = "nix-prefetch-git"; pkg = "nix-prefetch-scripts";}
        {cmd = "nix-prefetch-hg"; pkg = "nix-prefetch-scripts";}
        {cmd = "op"; pkg = "_1password"; chan = "unstable";}
        {cmd = "update-desktop-database"; pkg = "desktop-file-utils"; chan = "unstable";}
        {cmd = "lgogdownloader"; chan = "unstable";}
      ];
      onChange = "${pkgs.nur.repos.vdemeester.nr}/bin/nr default";
    };
  };
}
