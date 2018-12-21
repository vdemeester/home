{
  imports = [
    ./base.nix
  ];
  profiles.emacs = { enable = true; daemonService = false; };
}
