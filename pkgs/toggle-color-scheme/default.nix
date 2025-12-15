{
  lib,
  writeShellApplication,
  dconf,
}:
writeShellApplication {
  name = "toggle-color-scheme";
  runtimeInputs = [ dconf ];
  text = builtins.readFile ./toggle-color-scheme.sh;

  meta = {
    description = "Toggle GNOME color scheme between light and dark with niri integration";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
    mainProgram = "toggle-color-scheme";
  };
}
