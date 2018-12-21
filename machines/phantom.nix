{
  imports = [
    ./base.nix
  ];
  profiles = {
    dev = {
      go.enable = true;
      js.enable = true;
    };
    emacs = {
      enable = true;
      daemonService = false;
    };
  };
  programs = {
    vscode.enable = true;
  };
}
