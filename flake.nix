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
    home-manager-stable = { type = "github"; owner = "nix-community"; repo = "home-manager"; ref = "release-21.11"; inputs.nixpkgs.follows = "nixos-21_11"; };
    impermanence = { type = "github"; owner = "nix-community"; repo = "impermanence"; };

    nur.url = "github:nix-community/NUR";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    deploy-rs = {
      type = "github";
      owner = "serokell";
      repo = "deploy-rs";
      inputs.utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
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
    nix-hardware = { type = "github"; owner = "NixOS"; "repo" = "nixos-hardware"; };

    # Channels
    # FIXME: is it needed or should I just alias nixos-unstable instead
    nixpkgs = { type = "github"; owner = "NixOS"; repo = "nixpkgs"; ref = "nixos-unstable"; };
    nixos-21_11 = { type = "github"; owner = "NixOS"; repo = "nixpkgs"; ref = "nixos-21.11"; };
    nixos-unstable = { type = "github"; owner = "NixOS"; repo = "nixpkgs"; ref = "nixos-unstable"; };
    nixpkgs-unstable = { type = "github"; owner = "NixOS"; repo = "nixpkgs"; ref = "nixpkgs-unstable"; };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils-plus
    , flake-utils
    , home-manager
    , home-manager-stable
    , emacs-overlay
    , nur
    , sops-nix
    , envfs
    , nixos-wsl
    , nixos-hardware
    , deploy-rs
    , ...
    } @ inputs:
    let
      mkApp = flake-utils.lib.mkApp;
      # homeProfiles = import ./home { inherit (nixpkgs) lib; };

      nixosModules = flake-utils-plus.lib.exportModules [
        ./systems/modules/virtualisation/buildkit.nix
      ];
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
          # FIXME: migrate this to elsewhere, or at least split it
          ./systems/modules/core/config.nix
          ./systems/modules/core/nix.nix
          ./systems/modules/core/users.nix
          ./systems/modules/hardware/sane-extra-config.nixos.nix
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
          ./systems/modules/profiles/pulseaudio.nix
          ./systems/modules/profiles/qemu.nix
          ./systems/modules/profiles/redhat.nix
          ./systems/modules/profiles/scanning.nix
          ./systems/modules/profiles/ssh.nix
          ./systems/modules/profiles/syncthing.nix
          ./systems/modules/profiles/sway.nix
          ./systems/modules/profiles/virtualization.nix
          ./systems/modules/profiles/wireguard.server.nix
          ./systems/modules/profiles/yubikey.nix
          ./systems/modules/profiles/zsh.nix
          ./systems/modules/services/wireguard.client.nix
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
          modules = [
            home-manager.nixosModules.home-manager
            nixos-hardware.nixosModules.lenovo-thinkpad-t480s
            nixos-hardware.nixosModules.common-pc-laptop-ssd
            ./systems/modules/profiles/docker.nix
            ./systems/hosts/naruhodo.nix
          ];
        };
        # WSL setup
        # FIXME okinawa doesn't have openssh
        okinawa = {
          modules = [
            home-manager.nixosModules.home-manager
            ./systems/modules/profiles/docker.nix
            nixos-wsl.nixosModules.wsl
            ./systems/hosts/okinawa.nix
          ];
        };
        # Servers
        shikoku = {
          channelName = "nixos-21_11";
          modules = [
            home-manager-stable.nixosModules.home-manager
            ./systems/modules/profiles/docker.stable.nix
            ./systems/hosts/shikoku.nix
          ];
        };
        wakasu = {
          channelName = "nixos-21_11";
          modules = [
            home-manager-stable.nixosModules.home-manager
            nixos-hardware.nixosModules.lenovo-thinkpad
            nixos-hardware.nixosModules.common-pc-laptop-ssd
            ./systems/modules/profiles/docker.stable.nix
            ./systems/hosts/wakasu.nix
          ];
        };
        sakhalin = {
          channelName = "nixos-21_11";
          modules = [
            home-manager-stable.nixosModules.home-manager
            nixos-hardware.nixosModules.common-pc-ssd
            ./systems/hosts/sakhalin.nix
          ];
        };
        aomi = {
          channelName = "nixos-21_11";
          modules = [
            home-manager-stable.nixosModules.home-manager
            nixos-hardware.nixosModules.lenovo-thinkpad-p1-3th-gen
            nixos-hardware.nixosModules.common-pc-laptop-ssd
            ./systems/hosts/aomi.nix
          ];
        };
        kerkouane = {
          channelName = "nixos-21_11";
          modules = [
            home-manager-stable.nixosModules.home-manager
            ./systems/hosts/kerkouane.nix
          ];
        };
      };

      # deploy-rs setup
      deploy =
        let
          mkNode = server: ip: fast: {
            hostname = "${ip}";
            fastConnection = false;
            profiles.system.path =
              deploy-rs.lib.x86_64-linux.activate.nixos
                self.nixosConfigurations."${server}";
          };
        in
        {
          user = "root";
          sshUser = "root";
          nodes = {
            shikoku = mkNode "shikoku" "192.168.1.24" true;
            wakasu = mkNode "wakasu" "192.168.1.77" true;
          };
        };

      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;

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
                deploy-rs.packages."x86_64-linux".deploy-rs
              ];
            };
        };
    };
}
