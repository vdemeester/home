{...}:

{
  imports = [
    ./desktop.nix
  ];
  programs.autorandr = {
    enable = true;
  };
}
