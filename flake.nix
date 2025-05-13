{
  description = "System Config";

  outputs =
    { self, ... }@inputs:
    let
      inherit (self) outputs;
      stateVersion = "24.11";

      libx = import ./lib {
        inherit
          self
          inputs
          outputs
          stateVersion
          ;
      };

      stableModules = [ inputs.home-manager-24_11.nixosModules.home-manager ];
      unstableModules = [ inputs.home-manager.nixosModules.home-manager ];
      commonModules = [
        {
          config.nixpkgs.overlays = [
            (import ./nix/overlays/tekton.nix)
            (import ./nix/overlays/sbr.nix)
            inputs.emacs-overlay.overlay
            inputs.chapeau-rouge.overlays.openshift
            inputs.chick-group.overlays.default
            (_: prev: {
              inherit (inputs.buildkit-tekton.packages.${prev.system})
                tkn-local
                ;
              inherit (inputs.dagger.packages.${prev.system}) dagger;
            })
          ];
        }
        ./systems/modules/core/default.nix
        ./systems/modules/shell/default.nix
        ./systems/modules/desktop/default.nix
        ./systems/modules/dev/default.nix
        ./systems/modules/editors/default.nix
        ./systems/modules/hardware/default.nix
        ./systems/modules/profiles/default.nix
        ./systems/modules/virtualisation/default.nix
        ./systems/modules/virtualisation/buildkit.nix
        ./systems/modules/services/default.nix
        inputs.agenix.nixosModules.default
        # inputs.envfs.nixosModules.envfs
        {
          # config.nix.generateRegistryFromInputs = true;
          config.home-manager.useGlobalPkgs = true;
          config.home-manager.useUserPackages = true;
          # Import custom home-manager modules (NixOS)
          config.home-manager.sharedModules = import ./users/modules/modules.nix;
        }
      ];
    in
    {
      # Standalone home configurations
      # FIXME set this up
      homeConfigurations = {
        # headless machine
        "vincent@aion" = libx.mkHome {
          username = "vincent";
          hostname = "aion";
          system = "aarch64-linux";
        };
        "houbeb@aion" = libx.mkHome {
          username = "houbeb";
          hostname = "aion";
          system = "aarch64-linux";
        };
        # TODO vincent@honshu (darwin)
        # TODO vincent@okinawa (wsl ?)
      };
      nixosConfigurations = {
        # Work laptop (unstable)
        kyushu = libx.mkHost {
          hostname = "kyushu";
          desktop = "sway";
        };
        # Work workstation (unstable)
        # aomi = libx.mkhost {
        # 	hostname = "aomi";
        # };
        # # Servers (unstable)
        kobe = libx.mkHost { hostname = "kobe"; };
        athena = libx.mkHost {
          hostname = "athena";
          system = "aarch64-linux";
          pkgsInput = inputs.nixpkgs-24_11;
          homeInput = inputs.home-manager-24_11;
        };
        # shikoku = libx.mkHost { hostname = "shikoku"; };
        # FIXME migrate to libx.mkHost
        aomi = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules =
            commonModules
            ++ unstableModules
            ++ [
              inputs.nixos-hardware.nixosModules.lenovo-thinkpad-p1-gen3
              inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
              ./systems/hosts/aomi.nix
            ];
        };

        # Servers (stable)
        shikoku = inputs.nixpkgs-24_11.lib.nixosSystem {
          system = "x86_64-linux";
          modules = commonModules ++ stableModules ++ [ ./systems/hosts/shikoku.nix ];
        };
        sakhalin = inputs.nixpkgs-24_11.lib.nixosSystem {
          system = "x86_64-linux";
          modules =
            commonModules
            ++ stableModules
            ++ [
              inputs.nixos-hardware.nixosModules.common-pc-ssd
              ./systems/hosts/sakhalin.nix
            ];
        };
        kerkouane = inputs.nixpkgs-24_11.lib.nixosSystem {
          system = "x86_64-linux";
          modules =
            commonModules
            ++ stableModules
            ++ [
              ./systems/modules/services/govanityurl.nix
              ./systems/hosts/kerkouane.nix
            ];
        };
        # carthage = inputs.nixpkgs-24_11.lib.nixosSystem {
        #   system = "aarch64-linux";
        #   modules = commonModules ++ stableModules ++ [
        #     ./systems/hosts/carthage.nix
        #   ];
        # };
        # Raspberry PI
        # athena
        # athena = inputs.nixpkgs-24_11.lib.nixosSystem {
        #   system = "aarch64-linux";
        #   modules = commonModules ++ stableModules ++ [ ./systems/hosts/athena.nix ];
        # };
        # demeter
        demeter = inputs.nixpkgs-24_11.lib.nixosSystem {
          system = "aarch64-linux";
          modules = commonModules ++ stableModules ++ [ ./systems/hosts/demeter.nix ];
        };
      };

      nixosModules = {
        # provided modules (to be upstreamed)
        wireguard-client = ./modules/wireguard-client.nix;
      };

      # system-manager configurations
      # FIXME set this up
      systemConfigs = {
        aion = libx.mkSystemmanager {
          hostname = "aion";
          system = "aarch64-linux";
        };
      };

      images = {
        # ami(s) (AWS)
        carthage = inputs.nixos-generators.nixosGenerate rec {
          system = "aarch64-linux";
          format = "amazon";
          modules = commonModules ++ stableModules ++ [ ./systems/hosts/carthage.nix ];
        };
        # sdimages
        athena =
          (self.nixosConfigurations.athena.extendModules {
            modules = [
              "${inputs.nixpkgs-24_11}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
            ];
          }).config.system.build.sdImage;
        demeter =
          (self.nixosConfigurations.demeter.extendModules {
            modules = [
              "${inputs.nixpkgs-24_11}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
            ];
          }).config.system.build.sdImage;
      };
      # TODO: expose some packages ?
      # This is probably not gonna happen, instead I should move any internal package here outside, in their
      # own repository and flake. If they are useful upstream.

      overlays = import ./nix/overlays { inherit inputs; };

      devShells.x86_64-linux.default =
        let
          pkgs = import inputs.nixpkgs {
            system = "x86_64-linux";
            config.allowUnfree = true;
          };
        in
        pkgs.mkShell {
          packages = [
            pkgs.alejandra
            pkgs.git
            pkgs.nodePackages.prettier
            pkgs.deadnix
            pkgs.nixfmt-classic
            inputs.agenix.packages.x86_64-linux.default
          ];
          name = "home";
          DIRENV_LOG_FORMAT = "";
        };
    };

  inputs = {
    # Flake for compatibility with non-flake commands
    flake-compat = {
      type = "github";
      owner = "edolstra";
      repo = "flake-compat";
      flake = false;
    };

    buildkit-tekton = {
      url = "github:vdemeester/buildkit-tekton";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # nixpkgs
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-unstable";
    };
    nixpkgs-24_11 = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-24.11";
    };
    nixpkgs-master.url = "github:nixos/nixpkgs/master";
    # Home Manager
    home-manager = {
      type = "github";
      owner = "nix-community";
      repo = "home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager-24_11 = {
      type = "github";
      owner = "nix-community";
      repo = "home-manager";
      ref = "release-24.11";
      inputs.nixpkgs.follows = "nixpkgs-24_11";
    };

    # FIXME could still be useful for servers
    # impermanence = { type = "github"; owner = "nix-community"; repo = "impermanence"; };

    dagger = {
      type = "github";
      owner = "dagger";
      repo = "nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs-24_11";
    };

    # WSL
    nixos-wsl = {
      type = "github";
      owner = "nix-community";
      repo = "NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware = {
      type = "github";
      owner = "NixOS";
      "repo" = "nixos-hardware";
    };

    # Me :D
    chick-group = {
      type = "github";
      owner = "vdemeester";
      repo = "chick-group";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Red Hat
    chapeau-rouge = {
      type = "github";
      owner = "vdemeester";
      repo = "chapeau-rouge";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Used to generate NixOS images for other platforms
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix-24_11.url = "github:ryantm/agenix";
    agenix-24_11.inputs.nixpkgs.follows = "nixpkgs-24_11";

    lanzaboote.url = "github:nix-community/lanzaboote";
    lanzaboote.inputs.nixpkgs.follows = "nixpkgs";

    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    system-manager.url = "github:numtide/system-manager";
    system-manager.inputs.nixpkgs.follows = "nixpkgs";

    # nix-system-graphics.url = "github:soupglasses/nix-system-graphics";
    # nix-system-graphics.inputs.nixpkgs.follows = "nixpkgs";
  };
}
