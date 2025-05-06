# Shell for bootstrapping flake-enabled nix and home-manager
# Access development shell with  'nix develop' or (legacy) 'nix-shell'
# { pkgs ? (import ./nixpkgs.nix) { }
# ,
# }:
# {
#   default = pkgs.mkShell {
#     name = "home-flake";
#     # Enable experimental features without having to specify the argument
#     NIX_CONFIG = "experimental-features = nix-command flakes";
#     nativeBuildInputs = with pkgs; [
#       nix
#       home-manager
#       git
#     ];
#     shellHook = ''
#       exec zsh
#     '';
#   };
# }
{
  system ? builtins.currentSystem,
}:

# Use flake.nix devshell, similar to "nix develop"
(builtins.getFlake (toString ./.)).devShells.${system}.default
