inputs:

let
  inherit (inputs) self;
  inherit (inputs.home-manager.lib) homeManagerConfiguration;

  extraSpecialArgs = { inherit inputs; };

  defArgs = rec {
    configuration = { };
    system = "x86_64-linux";
    inherit extraSpecialArgs;
  };

  mkHome = args: homeManagerConfiguration (defArgs // args // {
    homeDirectory = "/home/${args.username}";
    pkgs = inputs.self.pkgs.${args.system or defArgs.system};
  });
in
{ inherit mkHome extraSpecialArgs; }
