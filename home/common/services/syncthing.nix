{ config
, lib
, pkgs
, outputs
, ...
}:
let
  isCurrentHost = n: v: n != config.networking.hostName;
  # Folders list
  folders = [
    # TODO maybe deprecate for documents
    { label = "sync"; id = "7dshg-r8zr6"; }
    { label = "documents"; id = "oftdb-t5anv"; }
    { label = "org"; id = "sjpsr-xfwdu"; }
    { label = "screenshots"; id = "prpsz-azlz9"; }
    { label = "wallpapers"; id = "wpiah-ydwwx"; }
    { label = "photos"; id = "uetya-ypa3d"; }
    { label = "music"; id = "kcyrf-mugzt"; }
  ];
  getSyncthingFolders = c:
    if builtins.hasAttr "syncthingFolders" c._module.specialArgs
    then
      c._module.specialArgs.syncthingFolders
    else
      [ ];
  deviceHasFolder = folder: n: v: lib.lists.any (s: s == folder) (getSyncthingFolders v);
  devicesForFolder = folder: lib.attrsets.filterAttrs (deviceHasFolder folder) outputs.nixosConfigurations;
  # outputs.nixosConfigurations.$.syncthingFolders will contains the folders for a host
  # FIXME: we could use another file, and a "configuration" so that I don't import it ? or in the flake, but not in makeHost

  # non-nixos syncthing machines
  extras = [
    # NAS
    {
      name = "aion";
      id = "YORNSGU-UC4IAG5-IWJCD7T-MVPIU7O-AYM36UK-LEHF7AP-CBC4L6C-ZWKUYQF";
      addresses = [ "tcp://aion.home" "tcp://aion.vpn" "tcp://aion.sbr.pm" ];
      folders = [ "org" "documents" "sync" "screenshots" "wallpapers" "photos" "videos" ];
    }
    # Macbook
    {
      name = "honshu";
      id = "RGIR34D-3SH3GZK-CYPNNFI-5M5I2K4-HVTUS56-72GJTLH-SDMOY4I-I7AURQR";
      addresses = [ "tcp://honshu.home" "tcp://honshu.sbr.pm" ];
      folders = [ ];
    }
    # Windows Gaming machine
    {
      name = "okinawa";
      id = "2RWT47Z-UGSH4QO-G4W6XN7-3XY722R-ZKGDN5U-4MDGHMA-6SM26QM-7VCQIAZ";
      addresses = [ "tcp://okinawa.home" "tcp://okinawa.vpn" "tcp://okinawa.sbr.pm" ];
      folder = [ ];
    }
    # iPhone
    {
      name = "hokkaido";
      id = "XD4XYNZ-DT3PJEY-UJYBHWX-6OQPPUI-HTW752L-FYTX3TW-GVHDTKW-PT336QV";
      folders = [ "org" "music" "documents" "sync" ];
    }
  ];
in
{
  services.syncthing = {
    enable = true;
    extraOptions = [ "--no-default-folder" ];
    # guiAddress = cfg.guiAddress;
    settings = { };
  };
}
