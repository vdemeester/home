# Custom packages, that can be defined similarly to ones from nixpkgs
# Build them using 'nix build .#example' or (legacy) 'nix-build -A example'

{ pkgs ? (import ../nixpkgs.nix) { }
,
}:
{
  # TODO: migrate things from nix/packages
  nixfmt-plus = pkgs.callPackage ./nixfmt-plus.nix { };
}
