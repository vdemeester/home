{ system ? builtins.currentSystem }:

# Use flake.nix devshell, similar to "nix develop"
(builtins.getFlake (toString /home/vincent/src/home)).devShells.${system}.tekton


