{
  globals,
  hostname,
  libx,
  ...
}:
{
  services.syncthing = {
    enable = true;
    user = "vincent";
    # FIXME: change this
    dataDir = "/home/vincent/.syncthing";
    configDir = "/home/vincent/.syncthing";
    guiAddress = libx.syncthingGuiAddress globals.machines."${hostname}";
    overrideFolders = false; # Just in case, will probably set to true later
    settings = {
      # FIXME this doesn't work, I wish it did.
      # defaults = {
      #   ignores = { lines = [ "(?d).DS_Store" "**" ]; };
      # };
      devices = libx.generateSyncthingDevices hostname globals.machines;
      folders =
        libx.generateSyncthingFolders hostname globals.machines."${hostname}" globals.machines
          globals.syncthingFolders;
    };
  };
}
