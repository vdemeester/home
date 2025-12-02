{
  config,
  hostname,
  libx,
  lib,
  ...
}:
let
  folders =
    libx.generateSyncthingFolders hostname config.osConfig.infrastructure.machines."${hostname}"
      config.osConfig.infrastructure.machines
      config.osConfig.infrastructure.syncthingFolders;
  folderNames = lib.mapAttrsToList (id: folder: folder.label or id) folders;
  folderList = lib.concatStringsSep ", " folderNames;
in
{
  warnings = [ "Home syncthing for ${hostname} with folders: ${folderList}" ];
  services.syncthing = {
    enable = true;
    overrideFolders = false; # Just in case, will probably set to true later
    guiAddress = libx.syncthingGuiAddress config.osConfig.infrastructure.machines."${hostname}";
    settings = {
      # FIXME this doesn't work, I wish it did.
      # defaults = {
      #   ignores = { lines = [ "(?d).DS_Store" "**" ]; };
      # };
      devices = libx.generateSyncthingDevices hostname config.osConfig.infrastructure.machines;
      folders = folders;
    };
  };
}
