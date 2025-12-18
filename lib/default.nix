{
  self,
  inputs,
  outputs,
  stateVersion,
  ...
}:
{
  libx = import ./functions.nix { inherit (inputs.nixpkgs) lib; };
  monitoring = import ./monitoring.nix { inherit (inputs.nixpkgs) lib; };
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
        libx = import ./functions.nix { inherit (inputs.nixpkgs) lib; };
      };
      modules = [
        ../home
      ];
    };

  newMkHost =
    {
      hostname,
      system ? "x86_64-linux",
      pkgsInput ? inputs.nixpkgs,
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
          globals
          system
          ;
        libx = import ./functions.nix { inherit (pkgsInput) lib; };
        monitoring = import ./monitoring.nix { inherit (pkgsInput) lib; };
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
        self.nixosModules.rsync-replica
        inputs.agenix.nixosModules.default
        ../systems/new.nix
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
        libx = import ./functions.nix { inherit (pkgsInput) lib; };
        monitoring = import ./monitoring.nix { inherit (pkgsInput) lib; };
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
        self.nixosModules.rsync-replica
        inputs.agenix.nixosModules.default
        inputs.lanzaboote.nixosModules.lanzaboote
        homeInput.nixosModules.home-manager
        {
          home-manager.extraSpecialArgs = specialArgs;
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
        }
        ../systems
      ];
    };

  # Function for generating host configs
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
          nixos-raspberrypi
          ;
        libx = import ./functions.nix { inherit (pkgsInput) lib; };
        monitoring = import ./monitoring.nix { inherit (pkgsInput) lib; };
      };
    in
    inputs.nixos-raspberrypi.lib.nixosSystemFull {
      inherit specialArgs;
      inherit system;
      modules = [
        (
          { ... }:
          {
            imports = with inputs.nixos-raspberrypi.nixosModules; [
              raspberry-pi-5.base
              raspberry-pi-5.bluetooth
              raspberry-pi-5.page-size-16k
              raspberry-pi-5.display-vc4
            ];
          }
        )
        self.nixosModules.wireguard-client
        self.nixosModules.wireguard-server
        self.nixosModules.govanityurl
        self.nixosModules.gosmee
        self.nixosModules.rsync-replica
        inputs.agenix.nixosModules.default
        inputs.lanzaboote.nixosModules.lanzaboote
        homeInput.nixosModules.home-manager
        {
          home-manager.extraSpecialArgs = specialArgs;
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
        }
        ../systems
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
        libx = import ./functions.nix { inherit (pkgsInput) lib; };
        monitoring = import ./monitoring.nix { inherit (pkgsInput) lib; };
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
}
