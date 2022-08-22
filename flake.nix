{
  description = "System Config";

  inputs = {
    # Flake for compatibility with non-flake commands
    flake-compat = { type = "github"; owner = "edolstra"; repo = "flake-compat"; flake = false; };
    flake-utils = { type = "github"; owner = "numtide"; repo = "flake-utils"; };
    flake-utils-plus = {
      type = "github";
      owner = "gytis-ivaskevicius";
      repo = "flake-utils-plus";
      ref = "v1.3.1";
      inputs.flake-utils.follows = "flake-utils";
    };
    devshell = { type = "github"; owner = "numtide"; repo = "devshell"; };

    # Flake Dependencies
    home-manager = { type = "github"; owner = "nix-community"; repo = "home-manager"; inputs.nixpkgs.follows = "nixpkgs"; };
    home-manager-21_11 = { type = "github"; owner = "nix-community"; repo = "home-manager"; ref = "release-21.11"; inputs.nixpkgs.follows = "nixos-21_11"; };
    home-manager-22_05 = { type = "github"; owner = "nix-community"; repo = "home-manager"; ref = "release-22.05"; inputs.nixpkgs.follows = "nixos-22_05"; };
    impermanence = { type = "github"; owner = "nix-community"; repo = "impermanence"; };
    nixpkgs-wayland = { type = "github"; owner = "nix-community"; repo = "nixpkgs-wayland"; inputs.nixpkgs.follows = "nixpkgs"; };

    nur.url = "github:nix-community/NUR";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

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
    nix-hardware = { type = "github"; owner = "NixOS"; "repo" = "nixos-hardware"; };

    # Channels
    # FIXME: is it needed or should I just alias nixos-unstable instead
    nixpkgs = { type = "github"; owner = "NixOS"; repo = "nixpkgs"; ref = "nixos-unstable"; };
    nixos-21_11 = { type = "github"; owner = "NixOS"; repo = "nixpkgs"; ref = "nixos-21.11"; };
    nixos-22_05 = { type = "github"; owner = "NixOS"; repo = "nixpkgs"; ref = "nixos-22.05"; };
    nixos-unstable = { type = "github"; owner = "NixOS"; repo = "nixpkgs"; ref = "nixos-unstable"; };
    nixpkgs-unstable = { type = "github"; owner = "NixOS"; repo = "nixpkgs"; ref = "nixpkgs-unstable"; };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils-plus
    , flake-utils
    , home-manager
    , home-manager-21_11
    , home-manager-22_05
    , emacs-overlay
    , nur
    , sops-nix
    , envfs
    , nixos-wsl
    , nixos-hardware
    , ...
    } @ inputs:
    let
      mkApp = flake-utils.lib.mkApp;

      nixosModules = flake-utils-plus.lib.exportModules [
        ./systems/modules/virtualisation/buildkit.nix
      ];

      stableModules_21_11 = [
        home-manager-21_11.nixosModules.home-manager
        ./systems/modules/profiles/docker.stable.nix
      ];
      stableModules_22_05 = [
        home-manager-22_05.nixosModules.home-manager
        ./systems/modules/profiles/docker.nix
      ];
      unstableModules = [
        home-manager.nixosModules.home-manager
        ./systems/modules/profiles/docker.nix
      ];
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
    in
    flake-utils-plus.lib.mkFlake {
      inherit self inputs nixosModules;

      supportedSystems = [ "aarch64-linux" "x86_64-linux" ];
      channelsConfig.allowUnfree = true;

      sharedOverlays = [
        (import ./nix/overlays)
        (import ./nix/overlays/sbr.nix)
        emacs-overlay.overlay
        nur.overlay
      ];

      hostDefaults = {
        system = "x86_64-linux";
        channelName = "nixos-unstable";
        extraArgs = {
          # nixos/profiles/core.nix requires self parameter
          inherit self;
        };
        modules = with nixosModules; [
          # Exported modules
          buildkit
          # Common modules
          ./systems/modules/core/default.nix
          ./systems/modules/desktop/default.nix
          ./systems/modules/dev/default.nix
          ./systems/modules/editors/default.nix
          ./systems/modules/hardware/default.nix
          ./systems/modules/profiles/default.flake.nix # TODO: rename
          # ./systems/modules/hardware/sane-extra-config.nixos.nix
          # FIXME: migrate this to elsewhere, or at least split it
          # Profiles probably need to go away
          ./systems/modules/profiles/avahi.nix
          ./systems/modules/profiles/base.nix
          ./systems/modules/profiles/builder.nix
          ./systems/modules/profiles/desktop.nix
          ./systems/modules/profiles/dev.nix
          ./systems/modules/profiles/dns.nix
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
          ./systems/modules/profiles/ssh.nix
          ./systems/modules/profiles/syncthing.nix
          ./systems/modules/profiles/sway.nix
          ./systems/modules/profiles/virtualization.nix
          ./systems/modules/profiles/wireguard.server.nix
          ./systems/modules/profiles/zsh.nix
          ./systems/modules/services/default.nix
          sops-nix.nixosModules.sops
          envfs.nixosModules.envfs
          {
            config.nix.generateRegistryFromInputs = true;
            config.home-manager.useGlobalPkgs = true;
            config.home-manager.useUserPackages = true;
            # Import custom home-manager modules (NixOS)
            config.home-manager.sharedModules = import ./users/modules/modules.nix;
            # Default SopsFile
            config.sops.defaultSopsFile = ./secrets/secrets.yaml;
          }
        ];
      };

      hosts = {
        # Main laptop
        naruhodo = {
          modules = unstableModules ++ [
            nixos-hardware.nixosModules.lenovo-thinkpad-t480s
            ./systems/hosts/naruhodo.nix
          ];
        };
        wakasu = {
          modules = unstableModules ++ [
            wayland
            nixos-hardware.nixosModules.lenovo-thinkpad-x1-9th-gen
            ./systems/hosts/wakasu.nix
          ];
        };
        # WSL setup
        # FIXME okinawa doesn't have openssh
        okinawa = {
          modules = unstableModules ++ [
            nixos-wsl.nixosModules.wsl
            ./systems/hosts/okinawa.nix
          ];
        };
        # Work "workstation"
        aomi = {
          # channelName = "nixos-22_05";
          modules = unstableModules ++ [
            nixos-hardware.nixosModules.lenovo-thinkpad-p1-gen3
            nixos-hardware.nixosModules.common-pc-laptop-ssd
            ./systems/hosts/aomi.nix
          ];
        };
        # Servers
        shikoku = {
          channelName = "nixos-22_05";
          modules = stableModules_22_05 ++ [
            ./systems/hosts/shikoku.nix
          ];
        };
        sakhalin = {
          channelName = "nixos-22_05";
          modules = stableModules_22_05 ++ [
            nixos-hardware.nixosModules.common-pc-ssd
            ./systems/hosts/sakhalin.nix
          ];
        };
        kerkouane = {
          channelName = "nixos-22_05";
          modules = stableModules_22_05 ++ [
            ./systems/modules/services/govanityurl.nix
            ./systems/hosts/kerkouane.nix
          ];
        };
      };

      # checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;

      outputsBuilder = channels:
        let
        in
        {
          overlays.default = import ./nix/overlays;

          packages = with channels.nixpkgs; {
            inherit
              bekind
              tkn
              tkn_0_23
              tkn_0_22
              tkn-pac
              tkn-pac_0_8
              tkn-pac_0_7
              tkn-pac_0_6
              tkn-local
              tkn-local_0_4
              tkn-local_0_3
              ;
          };

          # `nix develop`
          devShell =
            let
              inherit (sops-nix.packages."x86_64-linux") sops-import-keys-hook;
            in
            with channels.nixpkgs; mkShell {
              sopsPGPKeyDirs = [ "./secrets/keys" ];
              nativeBuildInputs = [
                sops-import-keys-hook
              ];
              buildInputs = with pkgs; [
                cachix
                git
                nixpkgs-fmt
                sops
              ];
            };
        };
    };
}
