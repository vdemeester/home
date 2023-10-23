{
  description = "System Config";

  outputs = { self, ... } @ inputs:
    let
      wayland = { pkgs, config, ... }: {
        config = {
          nix = {
            settings = {
              # add binary caches
              trusted-public-keys = [
                "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
              ];
              substituters = [ "https://nixpkgs-wayland.cachix.org" ];
            };
          };

          nixpkgs.overlays = [ inputs.nixpkgs-wayland.overlay ];
        };
      };
      stableModules = [
        inputs.home-manager-23_05.nixosModules.home-manager
      ];
      unstableModules = [
        inputs.home-manager.nixosModules.home-manager
      ];
      commonModules = [
        {
          config.nixpkgs.overlays = [
            (import ./nix/overlays).tekton
            (import ./nix/overlays/sbr.nix)
            inputs.emacs-overlay.overlay
            # inputs.nur.overlay
            # inputs.devshell.overlays.default
            inputs.chapeau-rouge.overlays.openshift
            (_: prev: {
              inherit (inputs.devenv.packages.${prev.system}) devenv;
            })
          ];
        }
        ./systems/modules/core/default.nix
        ./systems/modules/shell/default.nix
        ./systems/modules/desktop/default.nix
        ./systems/modules/dev/default.nix
        ./systems/modules/editors/default.nix
        ./systems/modules/hardware/default.nix
        ./systems/modules/profiles/default.flake.nix # TODO: rename
        ./systems/modules/virtualisation/default.nix
        ./systems/modules/virtualisation/buildkit.nix
        ./systems/modules/services/default.nix
        # FIXME: migrate this to elsewhere, or at least split it
        # Profiles probably need to go away
        ./systems/modules/profiles/base.nix
        ./systems/modules/profiles/builder.nix
        ./systems/modules/profiles/desktop.nix
        ./systems/modules/profiles/dev.nix
        ./systems/modules/profiles/dns.nix
        ./systems/modules/profiles/docker.nix
        ./systems/modules/profiles/git.nix
        ./systems/modules/profiles/gnome.nix
        ./systems/modules/profiles/home.nix
        ./systems/modules/profiles/i18n.nix
        ./systems/modules/profiles/i3.nix
        ./systems/modules/profiles/ipfs.nix
        ./systems/modules/profiles/kubernetes.nix
        ./systems/modules/profiles/laptop.nix
        ./systems/modules/profiles/mail.nix
        ./systems/modules/profiles/printing.nix
        ./systems/modules/profiles/qemu.nix
        ./systems/modules/profiles/redhat.nix
        ./systems/modules/profiles/scanning.nix
        ./systems/modules/profiles/virtualization.nix
        ./systems/modules/profiles/wireguard.server.nix
        ./systems/modules/profiles/zsh.nix
        inputs.sops-nix.nixosModules.sops
        inputs.envfs.nixosModules.envfs
        {
          # config.nix.generateRegistryFromInputs = true;
          config.home-manager.useGlobalPkgs = true;
          config.home-manager.useUserPackages = true;
          # Import custom home-manager modules (NixOS)
          config.home-manager.sharedModules = import ./users/modules/modules.nix;
          # Default SopsFile
          config.sops.defaultSopsFile = ./secrets/secrets.yaml;
        }
      ];
    in
    {
      nixosConfigurations =
        {
          # Work laptop (unstable)
          wakasu = inputs.nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = commonModules ++ unstableModules ++ [
              wayland
              inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-9th-gen
              ./systems/hosts/wakasu.nix
            ];
          };
          # Work workstation (unstable)
          aomi = inputs.nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = commonModules ++ unstableModules ++ [
              inputs.nixos-hardware.nixosModules.lenovo-thinkpad-p1-gen3
              inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
              ./systems/hosts/aomi.nix
            ];
          };
          # Servers (stable)
          shikoku = inputs.nixpkgs-23_05.lib.nixosSystem {
            system = "x86_64-linux";
            modules = commonModules ++ stableModules ++ [
              ./systems/hosts/shikoku.nix
            ];
          };
          sakhalin = inputs.nixpkgs-23_05.lib.nixosSystem {
            system = "x86_64-linux";
            modules = commonModules ++ stableModules ++ [
              inputs.nixos-hardware.nixosModules.common-pc-ssd
              ./systems/hosts/sakhalin.nix
            ];
          };
          kerkouane = inputs.nixpkgs-23_05.lib.nixosSystem {
            system = "x86_64-linux";
            modules = commonModules ++ stableModules ++ [
              ./systems/modules/services/govanityurl.nix
              ./systems/hosts/kerkouane.nix
            ];
          };
        };

      # TODO: expose some packages ?
      # This is probably not gonna happen, instead I should move any internal package here outside, in their
      # own repository and flake. If they are useful upstream.

      overlays = import ./nix/overlays;
    };

  inputs = {
    # Flake for compatibility with non-flake commands
    flake-compat = { type = "github"; owner = "edolstra"; repo = "flake-compat"; flake = false; };

    devenv = {
      url = "github:cachix/devenv/latest";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprland = {
      url = "github:hyprwm/Hyprland";
      inputs.nixpkgs.follows = "nixpkgs";
    };


    # nixpkgs
    nixpkgs = { type = "github"; owner = "NixOS"; repo = "nixpkgs"; ref = "nixos-unstable"; };
    nixpkgs-23_05 = { type = "github"; owner = "NixOS"; repo = "nixpkgs"; ref = "nixos-23.05"; };
    # Home Manager
    home-manager = { type = "github"; owner = "nix-community"; repo = "home-manager"; inputs.nixpkgs.follows = "nixpkgs"; };
    home-manager-23_05 = { type = "github"; owner = "nix-community"; repo = "home-manager"; ref = "release-23.05"; inputs.nixpkgs.follows = "nixpkgs-23_05"; };

    impermanence = { type = "github"; owner = "nix-community"; repo = "impermanence"; };

    nixpkgs-wayland = { type = "github"; owner = "nix-community"; repo = "nixpkgs-wayland"; inputs.nixpkgs.follows = "nixpkgs"; };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs-23_05";
    };

    sops-nix = {
      type = "github";
      owner = "Mic92";
      repo = "sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    envfs = {
      type = "github";
      owner = "Mic92";
      repo = "envfs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # WSL
    nixos-wsl = { type = "github"; owner = "nix-community"; repo = "NixOS-WSL"; inputs.nixpkgs.follows = "nixpkgs"; };
    nixos-hardware = { type = "github"; owner = "NixOS"; "repo" = "nixos-hardware"; };

    # Red Hat
    chapeau-rouge = {
      type = "github";
      owner = "vdemeester";
      repo = "chapeau-rouge";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
}
