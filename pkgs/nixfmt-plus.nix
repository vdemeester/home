{ pkgs, ... }:

pkgs.writeShellApplication {
  name = "nixfmt-plus";
  runtimeInputs = with pkgs; [
    deadnix
    nixfmt
    statix
  ];
  text = ''
    set -x
    deadnix --edit
    statix fix
    nixfmt .
  '';
}
