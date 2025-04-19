{ pkgs
, lib
, ...
}:
let
  swayRun = pkgs.writeShellScript "sway-run" ''
    export XDG_SESSION_TYPE=wayland
    export XDG_SESSION_DESKTOP=sway
    export XDG_CURRENT_DESKTOP=sway

    systemd-run --user --scope --collect --quiet --unit=sway systemd-cat --identifier=sway ${pkgs.sway}/bin/sway $@
  '';
in
{
  imports = [
    ./tiling-common.nix
  ];

  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    xwayland.enable = true;
  };

  # Allow swaylock to unlock the computer for us
  security.pam.services.swaylock = {
    text = "auth include login";
  };

  services.greetd.settings = {
    default_session = {
      # command = "${pkgs.greetd.greetd}/bin/agreety --cmd sway";
      command = "${lib.makeBinPath [ pkgs.greetd.tuigreet ]}/tuigreet -r --asterisks --time --cmd ${swayRun}";
      users = "greeter";
    };
    initial_session = {
      command = "${swayRun}";
      user = "vincent";
    };
  };
}
