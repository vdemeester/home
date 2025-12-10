{ writeShellApplication, dconf }:
writeShellApplication {
  name = "toggle-color-scheme";
  runtimeInputs = [ dconf ];
  text = builtins.readFile ./toggle-color-scheme.sh;
}
