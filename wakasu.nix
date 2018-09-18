{ pkgs, prefix, ...}:

{
  imports = [
    ./laptop.nix
    ./devops.nix
    ./dev.go.nix
    ./dev.rust.nix
    ./dev.python.nix
    ./dev.js.nix
    ./dev.java.nix
    ./dev.haskell.nix
  ];
  xdg.configFile."fish/conf.d/docker.fish".text = ''
    set -gx TESTKIT_AWS_KEYNAME "vdemeester-wakasu"
    set -gx DOCKER_BUILDKIT 1
  '';
  home.packages = with pkgs; [
    vscode
    jetbrains.idea-ultimate
    zoom-us
    debootstrap
    weechat weechat-xmpp
  ];
}
