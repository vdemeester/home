{
  globals,
  hostname,
  libx,
  ...
}:
{
  warnings = [ "Home syncthing for ${hostname}" ];
  services.syncthing = {
    enable = true;
    extraOptions = [ "--no-default-folder" ];
    overrideFolders = false; # Just in case, will probably set to true later
    guiAddress = libx.syncthingGuiAddress globals.machines."${hostname}";
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
