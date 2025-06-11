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
      devices = {
        aomi = {
          id = "XCR6WWB-OZUDGFB-LQPFW73-MV5SPJK-4IGOMA4-IAXON3I-C6OFETL-TPK5FQS";
          addresses = [
            "tcp://aomi.vpn"
            "tcp://aomi.light"
            "tcp://aomi.sbr.pm"
          ];
        };
        sakhalin = {
          id = "4TYYG7V-A67D5SN-HMEJCI7-POOZRLL-RNCIE4U-ZYVGTOB-JQ5DOSV-ZCGWUAL";
          addresses = [
            "tcp://sakhalin.light"
            "tcp://sakhalin.vpn"
            "tcp://sakhalin.sbr.pm"
          ];
        };
        shikoku = {
          id = "KZMMXRR-UINDQTS-H3TV2W7-EIGOUDI-3LW4ZDG-7PRKDFV-MJ5KUTJ-YG5Y5AI";
          addresses = [
            "tcp://shikoku.light"
            "tcp://shikoku.vpn"
            "tcp://shikoku.sbr.pm"
          ];
        };
        kerkouane = {
          id = "IFVRRQ7-KMIOQXP-5YDJXQU-UJXUKHB-7THCSY6-B3NHRNA-ED7IRI7-2JPPKQY";
          addresses = [
            "tcp://10.100.0.1"
            "tcp://kerkouane.vpn"
          ];
        };
        aion = {
          id = "YORNSGU-UC4IAG5-IWJCD7T-MVPIU7O-AYM36UK-LEHF7AP-CBC4L6C-ZWKUYQF";
          addresses = [
            "tcp://aion.light"
            "tcp://aion.vpn"
            "tcp://aion.sbr.pm"
          ];
        };
      };
      folders =
        globals.fn.generateSyncthingFolders globals.machines."${hostname}" globals.machines
          globals.syncthingFolders;
      # folders = {
      #   "/home/vincent/sync" = {
      #     label = "sync";
      #     id = "7dshg-r8zr6";
      #     devices = [
      #       "aomi"
      #       "aion"
      #       "shikoku"
      #       "sakhalin"
      #     ];
      #     rescanIntervalS = 3600 * 6;
      #   };
      #   "/home/vincent/desktop/org" = {
      #     label = "org";
      #     id = "sjpsr-xfwdu";
      #     devices = [
      #       "aomi"
      #       "aion"
      #       "shikoku"
      #       "sakhalin"
      #     ];
      #     rescanIntervalS = 3600 * 6;
      #   };
      #   "/home/vincent/desktop/documents" = {
      #     label = "documents";
      #     id = "oftdb-t5anv";
      #     devices = [
      #       "aomi"
      #       "aion"
      #       "shikoku"
      #       "sakhalin"
      #     ];
      #     rescanIntervalS = 3600 * 6;
      #   };
      #   "/home/vincent/desktop/pictures/screenshots" = {
      #     label = "screenshots";
      #     id = "prpsz-azlz9";
      #     devices = [
      #       "aomi"
      #       "aion"
      #       "shikoku"
      #       "sakhalin"
      #     ];
      #     rescanIntervalS = 3600 * 6;
      #   };
      #   "/home/vincent/desktop/pictures/wallpapers" = {
      #     label = "wallpapers";
      #     id = "wpiah-ydwwx";
      #     devices = [
      #       "aomi"
      #       "aion"
      #       "shikoku"
      #       "sakhalin"
      #     ];
      #     rescanIntervalS = 3600 * 6;
      #   };
      # };
    };
  };
}
