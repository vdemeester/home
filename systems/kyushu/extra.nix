{
  pkgs,
  lib,
  globals,
  ...
}:
{

  imports = [
    ../common/hardware/laptop.nix
    ../common/programs/direnv.nix
    ../common/programs/git.nix
    ../common/programs/tmux.nix
    ../common/services/networkmanager.nix
    ../common/services/containers.nix
    ../common/services/docker.nix
    ../common/services/lxd.nix

    ../redhat
  ];

  services = {
    # TODO probably migrate elsewhere
    kanata = {
      enable = true;
      package = pkgs.kanata-with-cmd;
      keyboards.x1 = {
        devices = [ "/dev/input/event0" ]; # internal keyboard
        config = builtins.readFile (./. + "/main.kbd");
        extraDefCfg = ''
          	danger-enable-cmd yes
            process-unmapped-keys yes
            override-release-on-activation yes
            concurrent-tap-hold yes
        '';
      };
    };
    dictd = {
      enable = true;
      DBs = with pkgs.dictdDBs; [
        wiktionary
        wordnet
        fra2eng
        eng2fra
      ];
    };
    locate = {
      enable = true;
      pruneBindMounts = true;
    };
    wireguard = {
      enable = true;
      ips = globals.fn.wg-ips globals.machines.kyushu.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
    };
    hardware.bolt.enable = true;
    printing = {
      enable = true;
      drivers = with pkgs; [
        cnijfilter2
        gutenprint
        gutenprintBin
      ];
    };
  };

  hardware.keyboard.qmk.enable = true;

  services.udev.packages = [ pkgs.sane-airscan ];
  hardware.sane = {
    enable = true;
    extraBackends = [ pkgs.sane-airscan ];
    openFirewall = true;
    netConf = "192.168.12.70";
  };

  environment.systemPackages = with pkgs; [
    kanata
    nixos-rebuild-ng
    go-org-readwise
    battery-monitor
    # Keyboard
    keymapp
    kontroll
    # backup
    restic # TODO: will probably move this to it's own configuration some day
    monolith # TODO: move into =desktop= ?
    virt-manager
    simple-scan
  ];

  # Make sure we don't start docker until required
  systemd.services.docker.wantedBy = lib.mkForce [ ];
  # Make sure we don't start lxd until required
  systemd.services.lxd.wantedBy = lib.mkForce [ ];

}
