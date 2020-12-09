# flake.nix --- the heart of my home
#
# Author:  Vincent Demeester <vincent@sbr.pm>
# URL:     https://git.srb.ht/~vdemeester/home
# License: GPLv3
#
# Welcome to ground zero. Where the whole flake gets set up and all its modules
# are loaded.
#
{
  description = ''
    home is the personal mono-repo of Vincent Demeester; containing the declarative configuration of
    servers, desktops, laptops - including dotfiles; a collection of packages; sources of several
    websites like vincent.demeester.fr, …
  '';

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "master";
    };
    nixos = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-20.09";
    };
    nixos-unstable = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-unstable";
    };
    nixos-hardware = {
      type = "github";
      owner = "NixOS";
      repo = "nixos-hardware";
      ref = "master";
    };
    # nix-darwin = {
    #   type = "github";
    #   owner = "LnL7";
    #   repo = "nix-darwin";
    #   ref = "master";
    # };
    home-manager = {
      type = "github";
      owner = "rycee";
      repo = "home-manager";
      ref = "master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs = {
      type = "github";
      owner = "nix-community";
      repo = "emacs-overlay";
      ref = "master";
    };
    gitignore-nix = {
      type = "github";
      owner = "hercules-ci";
      repo = "gitignore.nix";
      ref = "master";
      flake = false;
    };
    nyxt = {
      type = "github";
      owner = "atlas-engineer";
      repo = "nyxt";
      ref = "master";
      flake = false;
    };
  };

  outputs = { self, ... } @ inputs:
    with inputs.nixpkgs.lib;
    let
      forEachSystem = genAttrs [ "x86_64-linux" "aarch64-linux" ];

      # Packages
      mkPkgs = pkgs: system: import pkgs {
        inherit system;
        config = import ./nix/config.nix;
        overlays = self.internal.overlays."${system}";
      };
      unstablePkgsBySystem = forEachSystem (mkPkgs inputs.nixos-unstable);
      stablePkgsBySystem = forEachSystem (mkPkgs inputs.nixos);
      pkgsBySystem = forEachSystem (mkPkgs inputs.nixpkgs);

      # NixOS configurations
      mkNixOsConfiguration = name: { pkgs, system, config }:
        nameValuePair name (nixoSystem {
          inherit system;
          modules = [
            ({ name, ... }: {
              # Set the hostname to the name of the configuration being applied (since the
              # configuration being applied is determined by the hostname).
              networking.hostName = name;
            })
            ({ inputs, ... }: {
              # Use the nixpkgs from the flake.
              nixpkgs = { pkgs = pkgsBySystem."${system}"; };

              # For compatibility with nix-shell, nix-build, etc.
              environment.etc.nixpkgs.source = inputs.nixpkgs;
              nix.nixPath = [ "nixpkgs=/etc/nixpkgs" ];
            })
            ({ pkgs, ... }: {
              # Don't rely on the configuration to enable a flake-compatible version of Nix.
              nix = {
                package = pkgs.nixFlakes;
                extraOptions = "experimental-features = nix-command flakes";
              };
            })
            ({ lib, ... }: {
              # Set the system configuration revision.
              system.configurationRevision = lib.mkIf (self ? rev) self.rev;
            })
            ({ inputs, ... }: {
              # Re-expose self and nixpkgs as flakes.
              nix.registry = {
                self.flake = inputs.self;
                nixpkgs = {
                  from = { id = "nixpkgs"; type = "indirect"; };
                  flake = inputs.nixpkgs;
                };
              };
            })
            (import ./systems/modules)
            (import config)
          ];
          specialArgs = { inherit name inputs; };
        });

      # home-manager configurations
      mkHomeManagerConfiguration = name: { system, config }:
        nameValuePair name ({ ... }: {
          imports = [
            (import ./home/configs)
            (import ./home/modules)
            (import ./home/profiles)
            (import config)
          ];
          # For compatibility with nix-shell, nix-build, etc.
          home.file.".nixpkgs".source = inputs.nixpkgs;
          systemd.user.sessionVariables."NIX_PATH" =
            mkForce "nixpkgs=$HOME/.nixpkgs\${NIX_PATH:+:}$NIX_PATH";

          # Use the same Nix configuration throughout the system.
          xdg.configFile."nixpkgs/config.nix".source = ./nix/config.nix;

          # Re-expose self and nixpkgs as flakes.
          xdg.configFile."nix/registry.json".text = builtins.toJSON {
            version = 2;
            flakes =
              let
                toInput = input:
                  {
                    type = "path";
                    path = input.outPath;
                  } // (
                    filterAttrs
                      (n: _: n == "lastModified" || n == "rev" || n == "revCount" || n == "narHash")
                      input
                  );
              in
              [
                {
                  from = { id = "self"; type = "indirect"; };
                  to = toInput inputs.self;
                }
                {
                  from = { id = "nixpkgs"; type = "indirect"; };
                  to = toInput inputs.nixpkgs;
                }
              ];
          };
        });
    in
    {
      # `internal` isn't a known output attribute for flakes. It is used here to contain
      # anything that isn't meant to be re-usable.
      # Taken from davidtwco/veritas repository :)
      internal = {

        # Expose the development shells defined in the repository, run these with:
        #
        # nix develop 'self#devShells.x86_64-linux.cargo'
        devShells = forEachSystem (system:
          let
            pkgs = pkgsBySystem."${system}";
          in
          {
            # FIXME define your own here
            cargo = import ./nix/shells/cargo.nix { inherit pkgs; };
          }
        );

        # Attribute set of hostnames to home-manager modules with the entire configuration for
        # that host - consumed by the home-manager NixOS module for that host (if it exists)
        # or by `mkHomeManagerHostConfiguration` for home-manager-only hosts.
        homeManagerConfigurations = mapAttrs' mkHomeManagerConfiguration {
          naruhodo = { system = "x86_64-linux"; config = ./home/naruhodo.nix; };
        };

        # Overlays consumed by the home-manager/NixOS configuration.
        overlays = forEachSystem (system: [
          (self.overlay."${system}")
          (_: _: import inputs.gitignore-nix { lib = inputs.nixpkgs.lib; })
        ]);
      };

      # Attribute set of hostnames to be evaluated as NixOS configurations. Consumed by
      # `nixos-rebuild` on those hosts.
      nixosConfigurations = mapAttrs' mkNixOsConfiguration {
        naruhodo = { pkgs = inputs.nixos-unstable; system = "x86_64-linux"; config = ./systems/naruhodo.nix; };
        wakasu = { pkgs = inputs.nixos-unstable; system = "x86_64-linux"; config = ./systems/naruhodo.nix; };
        okinawa = { pkgs = inputs.nixos; system = "x86_64-linux"; config = ./systems/okinawa.nix; };
        sakhalin = { pkgs = inputs.nixos; system = "x86_64-linux"; config = ./systems/sakhalin.nix; };
        kerkouane = { pkgs = inputs.nixos; system = "x86_64-linux"; config = ./systems/kerkouane.nix; };
        # TODO raspberry pi 8G x 3 (name them too)
      };

      # Import the modules exported by this flake.
      # containerd, buildkit are interesting module to export from here
      nixosModules = {
        # FIXME move this to services
        containerd = import ./systems/modules/virtualisation/containerd.nix;
        buildkit = import ./systems/modules/virtualisation/buildkit.nix;
      };

      # Expose a dev shell which contains tools for working on this repository.
      devShell = forEachSystem
        (system:
          with pkgsBySystem."${system}";

          mkShell {
            name = "home";
            buildInputs = [
              cachix
              git-crypt
              nixpkgs-fmt
              gnumake
            ];
          }
        );

      # Expose an overlay which provides the packages defined by this repository.
      #
      # Overlays are used more widely in this repository, but often for modifying upstream packages
      # or making third-party packages easier to access - it doesn't make sense to share those,
      # so they in the flake output `internal.overlays`.
      #
      # These are meant to be consumed by other projects that might import this flake.
      overlay = forEachSystem (system: _: _: self.packages."${system}");

      # Expose the packages defined in this flake, built for any supported systems. These are
      # meant to be consumed by other projects that might import this flake.
      packages = forEachSystem
        (system:
          let
            pkgs = pkgsBySystem."${system}";
          in
          {
            apeStable = stablePkgsBySystem."${system}".callPackage ./pkgs/ape { };
            apeUnstable = unstablePkgsBySystem."${system}".callPackage ./pkgs/ape { };
            ape = pkgs.callPackage ./pkgs/ape { };
            nr = pkgs.callPackage ./pkgs/nr { };
            ram = pkgs.callPackage ./pkgs/ram { };
            systemd-email = pkgs.callPackage ./pkgs/systemd-email { };

            batzconverter = pkgs.callPackage ./pkgs/batzconverter { };
            # Tekton
            inherit (pkgs.callPackage ./pkgs/tkn { })
              tkn_0_11
              tkn_0_12
              tkn_0_13
              tkn_0_14
              tkn
              ;

            manifest-tool = pkgs.callPackage ./pkgs/manifest-tool { };
            ko = pkgs.callPackage ./pkgs/ko { };
          } // optionalAttrs (system == "x86_64-linux") {
            # OpenShift
            inherit (pkgs.callPackage ./pkgs/oc { })
              oc_4_1
              oc_4_2
              oc_4_3
              oc_4_4
              oc_4_5
              oc_4_6
              oc
              ;
            inherit (pkgs.callPackage ./pkgs/openshift-install { })
              openshift-install_4_3
              openshift-install_4_4
              openshift-install_4_5
              openshift-install_4_6
              openshift-install
              ;
          });

      # defaultPackage.x86_64-linux = self.packages.x86_64-linux.hello;

    };
}
