# flake.nix --- the heart of my home
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
      pkgsBySystem = forEachSystem
        (system:
          import inputs.nixpkgs {
            inherit system;
            config = import ./nix/config.nix;
            overlays = self.internal.overlays."${system}";
          }
        );
    in
    {
      # `internal` isn't a known output attribute for flakes. It is used here to contain
      # anything that isn't meant to be re-usable.
      # Taken from davidtwco/veritas repository :)
      internal = {
        # Overlays consumed by the home-manager/NixOS configuration.
        overlays = forEachSystem (system: [
          (self.overlay."${system}")
          (_: _: import inputs.gitignore-nix { lib = inputs.nixpkgs.lib; })
        ]);
      };

      # Attribute set of hostnames to be evaluated as NixOS configurations. Consumed by
      # `nixos-rebuild` on those hosts.
      # TODO naruhodo (hokkaido?) wakasu okinama sakhalin kerkouane
      # TODO raspberry pi 8G x 3 (name them too)
      nixosConfigurations = { };

      # Import the modules exported by this flake.
      # containerd, buildkit are interesting module to export from here
      nixosModules = { };

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
