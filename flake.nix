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
    , emacs-overlay
    , nur
    , sops-nix
    , envfs
    , nixos-wsl
    , ...
    } @ inputs:
    let
      mkApp = flake-utils.lib.mkApp;
      # homeProfiles = import ./home { inherit (nixpkgs) lib; };
    in
    flake-utils-plus.lib.mkFlake {
      inherit self inputs;

      supportedSystems = [ "aarch64-linux" "x86_64-linux" ];
      channelsConfig.allowUnfree = true;

      sharedOverlays = [
        (import ./nix/overlays)
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
        modules = [
          # Common modules
          ./systems/modules/default.flake.nix # FIXME rename to default.nix once all is migrated
          home-manager.nixosModules.home-manager
          sops-nix.nixosModules.sops
          envfs.nixosModules.envfs
          {
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
          modules = [ ./systems/hosts/naruhodo.nix ];
        };
        # WSL setup
        # FIXME okinawa doesn't have openssh
        okinawa = {
          modules = [
            nixos-wsl.nixosModules.wsl
            ./systems/hosts/okinawa.nix
          ];
        };
        # Servers
        shikoku = {
          channelName = "nixos-21_11";
          modules = [ ./systems/hosts/shikoku.nix ]; # Can add additionnal things
        };
        wakasu = {
          channelName = "nixos-21_11";
          modules = [ ./systems/hosts/wakasu.nix ]; # Can add additionnal things
        };
        sakhalin = {
          channelName = "nixos-21_11";
          modules = [ ./systems/hosts/sakhalin.nix ]; # Can add additionnal things
        };
        aomi = {
          channelName = "nixos-21_11";
          modules = [ ./systems/hosts/aomi.nix ]; # Can add additionnal things
        };
        kerkouane = {
          channelName = "nixos-21_11";
          modules = [ ./systems/hosts/kerkouane.nix ]; # Can add additionnal things
        };
      };

      # deploy-rs setup
      deploy = { };

      outputsBuilder = channels:
        let
        in
        {
          overlay = import ./nix/overlays;

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
