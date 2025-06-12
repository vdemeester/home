{
  globals,
  hostname,
  ...
}:
{
  services.syncthing = {
    enable = true;
    user = "vincent";
    # FIXME: change this
    dataDir = "/home/vincent/.syncthing";
    configDir = "/home/vincent/.syncthing";
    guiAddress = globals.fn.syncthingGuiAddress globals.machines."${hostname}";
    overrideFolders = false; # Just in case, will probably set to true later
    settings = {
      # FIXME this doesn't work, I wish it did.
      # defaults = {
      #   ignores = { lines = [ "(?d).DS_Store" "**" ]; };
      # };
      devices = globals.fn.generateSyncthingDevices globals.machines;
      folders =
        globals.fn.generateSyncthingFolders globals.machines."${hostname}" globals.machines
          globals.syncthingFolders;
    };
  };
}
