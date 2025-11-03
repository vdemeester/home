{
  description = "System Config";

  nixConfig = {
    extra-substituters = [
      "https://nixos-raspberrypi.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nixos-raspberrypi.cachix.org-1:4iMO9LXa8BqhU+Rpg6LQKiGa2lsNh/j2oiYLNOQ5sPI="
    ];
  };

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
    in
    {
      githubActions = inputs.nix-github-actions.lib.mkGithubMatrix {
        checks = inputs.nixpkgs.lib.getAttrs [ "x86_64-linux" ] self.packages;
      };
      githubActionsMatrix = builtins.toJSON (
        inputs.nixpkgs.lib.mapAttrsToList
          (name: value: {
            inherit name;
            arch = value._module.specialArgs.system;
          })
          (
            inputs.nixpkgs.lib.attrsets.filterAttrs (
              _: config: builtins.hasAttr "system" config._module.specialArgs
            ) self.nixosConfigurations
          )
      );
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
          # desktop = "sway";
          desktop = "niri";
        };
        # Test VM, name is..
        foobar = libx.mkHost {
          hostname = "foobar";
          desktop = "niri";
        };
        # Servers (unstable)
        aomi = libx.mkHost {
          hostname = "aomi";
        };
        sakhalin = libx.mkHost {
          hostname = "sakhalin";
        };
        # kobe = libx.mkHost {
        #   hostname = "kobe";
        # };
        # shikoku = libx.mkHost {
        #   hostname = "shikoku";
        # };
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
        # nagoya = libx.mkRPIHost {
        #   hostname = "nagoya";
        #   hardwareType = "rpi4"; # to disable tpm2 TODO find a better way
        # };
        nagoya = inputs.nixos-raspberrypi.lib.nixosSystem {
          specialArgs = inputs;
          modules = [
            {
              # Hardware specific configuration, see section below for a more complete
              # list of modules
              imports = with inputs.nixos-raspberrypi.nixosModules; [
                raspberry-pi-5.base
                raspberry-pi-5.page-size-16k
                raspberry-pi-5.display-vc4
                raspberry-pi-5.bluetooth
              ];
            }
            (
              {
                config,
                pkgs,
                ...
              }:
              {
                networking.hostName = "nagoya";
                boot.loader.raspberryPi.bootloader = "kernel";
                system.nixos.tags =
                  let
                    cfg = config.boot.loader.raspberryPi;
                  in
                  [
                    "raspberry-pi-${cfg.variant}"
                    cfg.bootloader
                    config.boot.kernelPackages.kernel.version
                  ];
                environment.systemPackages = with pkgs; [
                  tree
                ];
                fileSystems = {
                  "/boot/firmware" = {
                    device = "/dev/disk/by-uuid/2175-794E";
                    fsType = "vfat";
                    options = [
                      "noatime"
                      "noauto"
                      "x-systemd.automount"
                      "x-systemd.idle-timeout=1min"
                    ];
                  };
                  # "/" = {
                  #   device = "/dev/disk/by-uuid/44444444-4444-4444-8888-888888888888";
                  #   fsType = "ext4";
                  #   options = [ "noatime" ];
                  # };
                  "/" = {
                    device = "/dev/disk/by-uuid/e769fd8d-1fed-4a59-a987-e21f35294d5f";
                    fsType = "ext4";
                    options = [ "noatime" ];
                  };
                };
              }
            )
          ];
        };
        kerkouane = libx.mkHost {
          hostname = "kerkouane";
          pkgsInput = inputs.nixpkgs-25_05;
          homeInput = inputs.home-manager-25_05;
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
        aion = libx.mkSystemManager {
          hostname = "aion";
          system = "aarch64-linux";
        };
      };

      images = {
        # sdimages
        aix =
          (self.nixosConfigurations.aix.extendModules {
            modules = [
              "${inputs.nixpkgs-25_05}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
            ];
          }).config.system.build.sdImage;
        athena =
          (self.nixosConfigurations.athena.extendModules {
            modules = [
              "${inputs.nixpkgs-25_05}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
            ];
          }).config.system.build.sdImage;
        demeter =
          (self.nixosConfigurations.demeter.extendModules {
            modules = [
              "${inputs.nixpkgs-25_05}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
            ];
          }).config.system.build.sdImage;
        nagoya =
          (self.nixosConfigurations.nagoya.extendModules {
            modules = [
              "${inputs.nixpkgs-25_05}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
            ];
          }).config.system.build.sdImage;
      };

      overlays = import ./overlays { inherit inputs; };

      packages = forAllSystems (
        system:
        let
          pkgs = import inputs.nixpkgs {
            inherit system;
            config.allowAliases = false;
            overlays = [
              self.overlays.additions
            ];
          };
          skipDarwinPackages =
            system: n:
            if lib.strings.hasSuffix "darwin" system then !(lib.strings.hasPrefix "koff" n) else true;
          inherit (inputs.nixpkgs) lib;
          drvAttrs = builtins.filter (n: lib.isDerivation pkgs.${n} && skipDarwinPackages system n) (
            builtins.attrNames (self.overlays.additions pkgs pkgs)
          );
        in
        lib.listToAttrs (map (n: lib.nameValuePair n pkgs.${n}) drvAttrs)
      );

      checks = forAllSystems (system: {
        pre-commit-check = inputs.pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            # go
            gofmt.enable = true;
            # golangci-lint.enable = true;
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

    niri = {
      type = "github";
      owner = "sodiboo";
      repo = "niri-flake";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs-25_05";
    };

    dagger = {
      type = "github";
      owner = "dagger";
      repo = "nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs-25_05";
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

    lanzaboote.url = "github:nix-community/lanzaboote";
    lanzaboote.inputs.nixpkgs.follows = "nixpkgs";

    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    system-manager.url = "github:numtide/system-manager";
    system-manager.inputs.nixpkgs.follows = "nixpkgs";

    nix-github-actions.url = "github:nix-community/nix-github-actions";
    nix-github-actions.inputs.nixpkgs.follows = "nixpkgs";

    nixos-raspberrypi.url = "github:nvmd/nixos-raspberrypi/develop";

    code-cursor-nix.url = "github:jacopone/code-cursor-nix";
    code-cursor-nix.inputs.nixpkgs.follows = "nixpkgs";

    claude-code.url = "github:sadjow/claude-code-nix";
    claude-code.inputs.nixpkgs.follows = "nixpkgs";

    copilot-cli.url = "github:scarisey/copilot-cli-flake";
    copilot-cli.inputs.nixpkgs.follows = "nixpkgs";
  };
}
