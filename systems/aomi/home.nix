{ pkgs, config, ... }:
{
  imports = [
    ../../home/common/dev/default.nix
    ../../home/common/dev/emacs.nix
    ../../home/common/dev/containers.nix
    ../../home/common/dev/tektoncd.nix
    ../../home/common/desktop/passage.nix
  ];
  services.ssh-agent.enable = true;
  systemd.user.services.syncthing.Install.WantedBy = [ "multi-user.target" ];

  home.packages = with pkgs; [
    gnumake

    go-org-readwise
    gh-pr
    claude-hooks
  ];

  # Passage update service and timer
  systemd.user.services.passage-update = {
    Unit = {
      Description = "Update passage password store";
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.passage}/bin/passage git pull --rebase";
      WorkingDirectory = config.home.sessionVariables.PASSAGE_DIR;
    };
  };

  systemd.user.timers.passage-update = {
    Unit = {
      Description = "Daily passage password store update";
    };
    Timer = {
      OnCalendar = "daily";
      Persistent = true;
      RandomizedDelaySec = "1h";
    };
    Install = {
      WantedBy = [ "timers.target" ];
    };
  };
}
