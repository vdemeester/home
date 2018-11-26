{ pkgs, prefix, ...}:

{
  imports = [
    ./base.nix
  ];
  profiles.dev = {
    go.enable = true;
    js.enable = true;
  };
  programs = {
    vscode.enable = true;
    emacs.enable = {
      enable = true;
      daemonService = false;
    };
  };
}
