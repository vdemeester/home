{
  globals,
  hostname,
  ...
}:
{
  services.syncthing = {
    enable = true;
    extraOptions = [ "--no-default-folder" ];
    overrideFolders = false; # Just in case, will probably set to true later
    guiAddress = globals.fn.syncthingGuiAddress globals.machines."${hostname}";
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
