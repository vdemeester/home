{
  imports = [
    ./base.nix
  ];
  profiles.laptop.enable = true;
  profiles.dev = {
    go.enable = true;
    rust.enable = true;
  };
  profiles.gpg.enable = true;
  profiles.containers.enable = true;
  profiles.media.enable = true;
  programs.podman.enable = true;
}
