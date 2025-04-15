{ self
, inputs
, outputs
, stateVersion
, ...
}:
{
  # Function for generating home-manage configs
  mkHome =
    { hostname
    , user
    , desktop ? null
    , system ? "x86_64-linux"
    ,
    }:
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
          ;
        username = user;
      };
      modules = [
        # ../home
      ];
    };

  # Function for generating host configs
  mkHost =
    { hostname
    , desktop ? null
    , system ? "x86_64-linux"
    , pkgsInput ? inputs.nixpkgs
    , homeInput ? inputs.home-manager
    ,
    }:
    pkgsInput.lib.nixosSystem {
      specialArgs = {
        inherit
          self
          inputs
          outputs
          stateVersion
          hostname
          desktop
          system
          ;
      };
      system = system;
      modules = [
        inputs.agenix.nixosModules.default
        inputs.lanzaboote.nixosModules.lanzaboote
        homeInput.nixosModules.home-manager
        ../systems
      ];
    };

  # Function to create a system manager
  mkSystemManager =
    { hostname
    , system ? "x86_64-linux"
    ,
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
