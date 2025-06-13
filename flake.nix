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

      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = inputs.nixpkgs.lib.genAttrs supportedSystems;

      stableModules = [ inputs.home-manager-24_11.nixosModules.home-manager ];
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
        # Servers (unstable)
        aomi = libx.mkHost {
          hostname = "aomi";
        };
        kobe = libx.mkHost {
          hostname = "kobe";
        };
        # Servers (stable)
        athena = libx.mkHost {
          hostname = "athena";
          system = "aarch64-linux";
          hardwareType = "rpi4";
          pkgsInput = inputs.nixpkgs-25_05;
          homeInput = inputs.home-manager-25_05;
        };
        demeter = libx.mkHost {
          hostname = "demeter";
          system = "aarch64-linux";
          hardwareType = "rpi4";
          pkgsInput = inputs.nixpkgs-25_05;
          homeInput = inputs.home-manager-25_05;
        };
        aix = libx.mkHost {
          hostname = "aix";
          system = "aarch64-linux";
          hardwareType = "rpi4";
          pkgsInput = inputs.nixpkgs-25_05;
          homeInput = inputs.home-manager-25_05;
        };
        # shikoku = libx.mkHost { hostname = "shikoku"; };
        # sakhalin = libx.mkHost { hostname = "sakhalin"; };
        kerkouane = libx.mkHost {
          hostname = "kerkouane";
          pkgsInput = inputs.nixpkgs-25_05;
          homeInput = inputs.home-manager-25_05;
        };
        # FIXME migrate to libx.mkHost
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
      };

      nixosModules = {
        # provided modules (to be upstreamed)
        wireguard-client = ./modules/wireguard-client.nix;
        wireguard-server = ./modules/wireguard-server.nix;
        govanityurl = ./modules/govanityurl.nix;
        gosmee = ./modules/gosmee.nix;
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
        # sdimages
        aix =
          (self.nixosConfigurations.aix.extendModules {
            modules = [
              "${inputs.nixpkgs-24_11}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
            ];
          }).config.system.build.sdImage;
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

      checks = forAllSystems (system: {
        pre-commit-check = inputs.pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            # go
            gofmt.enable = true;
            golangci-lint.enable = true;
            # nix
            deadnix.enable = true;
            nixfmt-rfc-style.enable = true;
            # statix.enable = true;
            # python
            flake8.enable = true;
            ruff.enable = true;
            # shell
            shellcheck.enable = true;
          };
        };
      });

      devShells = forAllSystems (system: {
        default =
          let
            pkgs = import inputs.nixpkgs {
              inherit system;
              config.allowUnfree = true;
            };
          in
          inputs.nixpkgs.legacyPackages.${system}.mkShell {
            inherit (self.checks.${system}.pre-commit-check) shellHook;
            buildInputs = self.checks.${system}.pre-commit-check.enabledPackages;
            packages = [
              pkgs.git
              pkgs.nodePackages.prettier
              pkgs.deadnix
              pkgs.nixfmt-rfc-style
              inputs.agenix.packages.${system}.default
            ];
            name = "home";
            DIRENV_LOG_FORMAT = "";
          };
      });
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
    nixpkgs-25_05 = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-25.05";
    };
    nixpkgs-24_11 = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-24.11";
    };
    nixpkgs-master.url = "github:nixos/nixpkgs/master";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    # Home Manager
    home-manager = {
      type = "github";
      owner = "nix-community";
      repo = "home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager-25_05 = {
      type = "github";
      owner = "nix-community";
      repo = "home-manager";
      ref = "release-25.05";
      inputs.nixpkgs.follows = "nixpkgs-25_05";
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
    agenix-25_05.url = "github:ryantm/agenix";
    agenix-25_05.inputs.nixpkgs.follows = "nixpkgs-25_05";
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
