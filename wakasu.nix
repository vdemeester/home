{ pkgs, prefix, ...}:

{
  imports = [
    ./desktop.nix
    ./devops.nix
    ./openshift.nix
  ];
  profiles.laptop.enable = true;
  profiles.dev = {
    go.enable = true;
    java.enable = true;
    js.enable = true;
    haskell.enable = true;
    python.enable = true;
    rust.enable = true;
  };
  programs.vscode.enable = true;
  programs.podman.enable = true;
  xdg.configFile."fish/conf.d/docker.fish".text = ''
    set -gx DOCKER_BUILDKIT 1
  '';
  home.packages = with pkgs; [
    google-chrome
    obs-studio
    mattermost-desktop
    slack
    virtmanager
    jetbrains.idea-ultimate
  ];
  services.shairport-sync.enable = true;
}
