{ lib, ... }:
{
  options.infrastructure.syncthingFolders = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule {
        options = {
          id = lib.mkOption {
            type = lib.types.str;
            description = "Syncthing folder ID";
          };

          path = lib.mkOption {
            type = lib.types.str;
            description = "Default path for this syncthing folder";
          };
        };
      }
    );
    default = {
      sync = {
        id = "7dshg-r8zr6";
        path = "/home/vincent/sync";
      };
      documents = {
        id = "oftdb-t5anv";
        path = "/home/vincent/desktop/documents";
      };
      org = {
        id = "sjpsr-xfwdu";
        path = "/home/vincent/desktop/org";
      };
      screenshots = {
        id = "prpsz-azlz9";
        path = "/home/vincent/desktop/pictures/screenshots";
      };
      wallpapers = {
        id = "wpiah-ydwwx";
        path = "/home/vincent/desktop/pictures/wallpapers";
      };
      photos = {
        id = "uetya-ypa3d";
        path = "/home/vincent/desktop/pictures/photos";
      };
      music = {
        id = "kcyrf-mugzt";
        path = "/home/vincent/desktop/music";
      };
    };
    description = "Syncthing folder definitions";
  };
}
