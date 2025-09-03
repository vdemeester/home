{
  pkgs,
  lib,
  ...
}:
let
  niriRun = pkgs.writeShellScript "niri-run" ''
    export XDG_SESSION_TYPE=wayland
    export XDG_SESSION_DESKTOP=niri
    export XDG_CURRENT_DESKTOP=niri

    systemd-run --user --scope --collect --quiet --unit=niri systemd-cat --identifier=niri ${pkgs.niri}/bin/niri $@
  '';
in
{
  imports = [
    ./tiling-common.nix
  ];

  programs.niri = {
    enable = true;
  };
  # Allow swaylock to unlock the computer for us
  security.pam.services.swaylock = {
    text = "auth include login";
  };

  services.greetd.settings = {
    default_session = {
      # command = "${pkgs.greetd.greetd}/bin/agreety --cmd niri";
      command = "${
        lib.makeBinPath [ pkgs.greetd.tuigreet ]
      }/tuigreet -r --asterisks --time --cmd ${niriRun}";
      users = "greeter";
    };
    initial_session = {
      command = "${niriRun}";
      user = "vincent";
    };
  };
}
