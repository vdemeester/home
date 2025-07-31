{
  self,
  inputs,
  outputs,
  stateVersion,
  ...
}:
let
  # Helper function to create common special args
  mkSpecialArgs = {
    hostname,
    desktop ? null,
    hardwareType ? "",
    system ? "x86_64-linux",
    pkgsInput ? inputs.nixpkgs,
    nixos-raspberrypi ? null,
  }:
    let
      globals = import ../globals.nix {
        inherit (pkgsInput) lib;
        inherit hostname;
      };
    in
    {
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
    } // (if nixos-raspberrypi != null then { inherit nixos-raspberrypi; } else { });

  # Helper function to create common modules list
  mkCommonModules = { homeInput, withRPIModules ? false }: [
    self.nixosModules.wireguard-client
    self.nixosModules.wireguard-server
    self.nixosModules.govanityurl
    self.nixosModules.gosmee
    inputs.agenix.nixosModules.default
    inputs.lanzaboote.nixosModules.lanzaboote
    homeInput.nixosModules.home-manager
    ../systems
  ] ++ (if withRPIModules then [
    (
      { ... }:
      {
        imports = with inputs.nixos-raspberrypi.nixosModules; [
          raspberry-pi-5.base
          raspberry-pi-5.bluetooth
        ];
      }
    )
  ] else []);

  # System type configurations with defaults
  systemTypes = {
    rpi4 = {
      system = "aarch64-linux";
      hardwareType = "rpi4";
      pkgsInput = inputs.nixpkgs-25_05;
      homeInput = inputs.home-manager-25_05;
    };
    
    server = {
      system = "x86_64-linux";
      pkgsInput = inputs.nixpkgs;
      homeInput = inputs.home-manager;
    };
    
    laptop = {
      system = "x86_64-linux";
      pkgsInput = inputs.nixpkgs;
      homeInput = inputs.home-manager;
    };
  };
in
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

  # Function for generating host configs with system type support
  mkHost =
    {
      hostname,
      desktop ? null,
      hardwareType ? "",
      system ? "x86_64-linux",
      pkgsInput ? inputs.nixpkgs,
      homeInput ? inputs.home-manager,
      systemType ? null,
    }:
    let
      # Apply system type defaults if specified
      config = if systemType != null && systemTypes ? ${systemType}
        then systemTypes.${systemType} // { inherit hostname desktop hardwareType; }
        else { inherit hostname desktop hardwareType system pkgsInput homeInput; };
      
      specialArgs = mkSpecialArgs config;
      modules = mkCommonModules { homeInput = config.homeInput; };
    in
    config.pkgsInput.lib.nixosSystem {
      inherit specialArgs;
      system = config.system;
      modules = modules ++ [
        { home-manager.extraSpecialArgs = specialArgs; }
      ];
    };

  # Function for generating RPi host configs (kept for backward compatibility)
  mkRPIHost =
    {
      hostname,
      desktop ? null,
      hardwareType ? "",
      pkgsInput ? inputs.nixpkgs-25_05,
      homeInput ? inputs.home-manager-25_05,
      nixos-raspberrypi ? inputs.nixos-raspberrypi,
    }:
    let
      system = "aarch64-linux";
      specialArgs = mkSpecialArgs {
        inherit hostname desktop hardwareType system pkgsInput;
        nixos-raspberrypi = nixos-raspberrypi;
      };
      modules = mkCommonModules { homeInput = homeInput; withRPIModules = true; };
    in
    inputs.nixos-raspberrypi.lib.nixosSystemFull {
      inherit specialArgs system;
      modules = modules ++ [
        { home-manager.extraSpecialArgs = specialArgs; }
      ];
    };

  # Function to create a system manager
  mkSystemManager =
    {
      system ? "x86_64-linux",
      hostname,
      desktop ? null,
      pkgsInput ? inputs.nixpkgs,
      homeInput ? inputs.home-manager,
    }:
    let
      globals = import ../globals.nix {
        inherit (pkgsInput) lib;
        inherit hostname;
      };
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
      };
    in
    inputs.system-manager.lib.makeSystemConfig {
      inherit extraSpecialArgs;
      modules = [
        # self.nixosModules.wireguard-client
        # inputs.agenix.nixosModules.default
        homeInput.nixosModules.home-manager
        {
          config = {
            nixpkgs.hostPlatform = system;
            system-manager.allowAnyDistro = true;
          };
        }
        ../systems/system-manager.nix
      ];
    };

  # Expose system types for external use
  inherit systemTypes;
}
