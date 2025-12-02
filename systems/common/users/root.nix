{
  config,
  lib,
  ...
}:
let
  shikokuKey = config.infrastructure.machines.shikoku.ssh.vincent;
in
{
  users.users.root = {
    openssh.authorizedKeys.keys =
      config.infrastructure.users.vincent.sshKeys ++ lib.optionals (shikokuKey != null) [ shikokuKey ];
  };
}
