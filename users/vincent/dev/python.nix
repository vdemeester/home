{ pkgs, ... }:

{
  home.packages = with pkgs; [
    python3
    python36Packages.virtualenv
    python36Packages.pip-tools
    python36Packages.tox
    pipenv
  ];
}
