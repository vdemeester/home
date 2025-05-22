{
  globals,
  ...
}:
{
  users.users.root = {
    openssh.authorizedKeys.keys = globals.ssh.vincent ++ [ globals.machines.shikoku.ssh.vincent ];
  };
}
