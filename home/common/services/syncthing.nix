{
  globals,
  hostname,
  libx,
  lib,
  ...
}:
let
  folders =
    libx.generateSyncthingFolders hostname globals.machines."${hostname}" globals.machines
      globals.syncthingFolders;
  folderNames = lib.mapAttrsToList (id: folder: folder.label or id) folders;
  folderList = lib.concatStringsSep ", " folderNames;
in
{
  warnings = [ "Home syncthing for ${hostname} with folders: ${folderList}" ];
  services.syncthing = {
    enable = true;
    overrideFolders = false; # Just in case, will probably set to true later
    guiAddress = libx.syncthingGuiAddress globals.machines."${hostname}";
    settings = {
      # FIXME this doesn't work, I wish it did.
      # defaults = {
      #   ignores = { lines = [ "(?d).DS_Store" "**" ]; };
      # };
      devices = libx.generateSyncthingDevices hostname globals.machines;
      folders = folders;
    };
  };
}
