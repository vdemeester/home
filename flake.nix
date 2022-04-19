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
    , ...
    } @ inputs:
    let
      mkApp = flake-utils.lib.mkApp;
      homeProfiles = import ./home { inherit (nixpkgs) lib; };
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
          ./systems/modules
          home-manager.nixosModules.home-manager
          {
            # Import custom home-manager modules (NixOS)
            config.home-manager.sharedModules = import ./users/modules/modules.nix;
          }
        ];
      };

      hosts = {
        naruhodo = {
          modules = [ ./systems/hosts/naruhodo.nix ];
        };
        okinawa = {
          modules = [ ./systems/hosts/okinawa.nix ];
        };
        shikoku = {
          channelName = "nixos-21_11";
          modules = [ ./systems/hosts/shikoku.nix ];
        };
      };

      outputsBuilder = channels:
        let
        in
        {
          overlay = import ./nix/overlays;
          devShell = with channels.nixpkgs; mkShell {
            sopsPGPKeyDirs = [ "./secrets/keys" ];
            nativeBuildInputs = [
              (pkgs.callPackage pkgs.sops-nix { }).sops-import-keys-hook
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
