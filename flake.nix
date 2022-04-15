{
  description = "System Config";

  inputs = {
    # Flake for compatibility with non-flake commands
    flake-compat = { type = "github"; owner = "edolstra"; repo = "flake-compat"; flake = false; };
    flake-utils = { type = "github"; owner = "numtide"; repo = "flake-utils"; };
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

  outputs = { self, nixpkgs, nixos-21_11, ... }@inputs:
    let
      lib = import ./nix/lib inputs;
      inherit (lib) genSystems;

      overlays.default = import ./nix/packages;

      pkgs = genSystems
        (system:
          import nixpkgs {
            inherit system;
            overlays = [
              inputs.devshell.overlay
              inputs.emacs-overlay.overlay
              overlays.default
            ];

            config.allowUnfree = true;
          });
      stablePkgs = genSystems
        (system:
          import nixos-21_11 {
            inherit system;
            overlays = [
              inputs.devshell.overlay
              inputs.emacs-overlay.overlay
              overlays.default
            ];

            config.allowUnfree = true;
          });
    in
    {
      inherit lib overlays pkgs;

      # Standalone home-manager config
      inherit (import ./home/profiles inputs) homeConfigurations;

      deploy = import ./hosts/deploy.nix inputs;

      # NixOS configuration with home-manager
      nixosConfigurations = import ./systems/hosts inputs;

      # devShells = genSystems (system: {
      #   default = pkgs.${system}.devshell.mkShell {
      #     packages = with pkgs.${system}; [
      #       git
      #       nixpkgs-fmt
      #       inputs.deploy-rs.defaultPackage.${system}
      #       # repl
      #     ];
      #     name = "dots";
      #   };
      # });

      packages = lib.genAttrs [ "x86_64-linux" ] (system: {
        inherit (pkgs.${system})
          ;
      });
    };
}
