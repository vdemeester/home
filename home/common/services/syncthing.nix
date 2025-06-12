{
  globals,
  hostname,
  ...
}:
{
  # warning = [ "${ttt}" ];
  services.syncthing = {
    enable = true;
    extraOptions = [ "--no-default-folder" ];
    # guiAddress = cfg.guiAddress;
    # TODO This is only for kyushu, will need to migrate this later
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
