{ pkgs, config, lib, ...}:

{
  programs.fish = {
    enable = true;
    shellAbbrs = {
      gs = "git status";
    };
    shellAliases = import ./aliases.nix;
  };
  xdg.configFile."fish/conf.d/nix-aliases.fish".source = ./fish/nix-aliases.fish;
}
