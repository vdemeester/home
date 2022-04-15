inputs:

{
  nodes = with inputs.deploy-rs-lib.x86_64-linux; {
    aomi = { };
    sakhalin = { };
    shikoku = {
      hostname = "shikoku.home";
      profiles.system = {
        user = "root";
        path = activate.nixos inputs.self.nixosConfigurations.shikoku;
      };
    };
    wakasu = { };
  };
}
