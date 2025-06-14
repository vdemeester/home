{
  self,
  inputs,
  outputs,
  stateVersion,
  ...
}:
{
  # Function for generating home-manage configs
  mkHome =
    {
      hostname,
      user,
      desktop ? null,
      system ? "x86_64-linux",
    }:
    let
      globals = import ../globals.nix {
        inherit (inputs.nixpkgs) lib;
        inherit hostname;
      };
    in
    inputs.home-manager.lib.homeManagerConfiguration {
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      extraSpecialArgs = {
        inherit
          self
          inputs
          outputs
          stateVersion
          hostname
          desktop
          globals
          ;
        username = user;
      };
      modules = [
        ../home
      ];
    };

  # Function for generating host configs
  mkHost =
    {
      hostname,
      desktop ? null,
      hardwareType ? "",
      system ? "x86_64-linux",
      pkgsInput ? inputs.nixpkgs,
      homeInput ? inputs.home-manager,
    }:
    let
      globals = import ../globals.nix {
        inherit (pkgsInput) lib;
        inherit hostname;
      };
      specialArgs = {
        inherit
          self
          inputs
          outputs
          stateVersion
          hostname
          desktop
          hardwareType
          system
          globals
          ;
      };
    in
    pkgsInput.lib.nixosSystem {
      inherit specialArgs;
      inherit system;
      modules = [
        self.nixosModules.wireguard-client
        self.nixosModules.wireguard-server
        self.nixosModules.govanityurl
        self.nixosModules.gosmee
        inputs.agenix.nixosModules.default
        inputs.lanzaboote.nixosModules.lanzaboote
        homeInput.nixosModules.home-manager
        { home-manager.extraSpecialArgs = specialArgs; }
        ../systems
      ];
    };

  # Function to create a system manager
  mkSystemManager =
    {
      system ? "x86_64-linux",
    }:
    inputs.system-manager.lib-makeSystemConfig {
      modules = [
        # ../modules ?
        {
          config = {
            nixpkgs.hostPlatform = system;
            system-manager.allowAnyDistro = true;
          };
        }
      ];
    };
}
