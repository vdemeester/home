{ ... }:

# This file just imports all the modules defined in this folder.

{
  imports = [
    ./buildkit.nix
    ./containerd.nix
  ];
}
