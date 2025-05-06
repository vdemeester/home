_: {
  services.syncthing = {
    enable = true;
    user = "vincent";
    dataDir = "/home/vincent/.syncthing";
    configDir = "/home/vincent/.syncthing";
    # guiAddress = cfg.guiAddress;
    settings = { };
  };
}
