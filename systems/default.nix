{
  config,
  desktop,
  hostname,
  inputs,
  lib,
  outputs,
  stateVersion,
  ...
}:
{

  imports =
    [
      (./. + "/${hostname}/boot.nix")
      (./. + "/${hostname}/hardware.nix")

      ./common/base
      ./common/users
    ]
    ++ lib.optional (builtins.pathExists (./. + "/${hostname}/extra.nix")) ./${hostname}/extra.nix
    ++ lib.optional (builtins.isString desktop) ./common/desktop;

  nixpkgs = {
    overlays = [
      # Our own flake exports (from overlays and pkgs dir)
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages

      # And from other flakes
      inputs.emacs-overlay.overlay
      inputs.chapeau-rouge.overlays.openshift
      inputs.chick-group.overlays.default
      inputs.agenix.overlays.default

      (import ../nix/overlays/sbr.nix)

      # Migrate to "modifications"
      (_: prev: {
        inherit (inputs.buildkit-tekton.packages.${prev.system}) tkn-local;
        inherit (inputs.dagger.packages.${prev.system}) dagger;
      })
    ];
    config = {
      allowUnfree = true;
    };
  };

  nix = {
    # This will add each flake input as a registry
    # To make nix3 commands consistent with your flake
    registry = lib.mkForce (lib.mapAttrs (_: value: { flake = value; }) inputs);

    # This will additionally add your inputs to the system's legacy channels
    # Making legacy nix commands consistent as well, awesome!
    nixPath = lib.mkForce (
      lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry
    );

    optimise = {
      automatic = true;
      dates = [
        "01:10"
        "12:10"
      ];
    };

    settings = {
      auto-optimise-store = true;
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      sandbox = true;
      allowed-users = [
        "@wheel"
      ];
      trusted-users = [
        "root"
        "@wheel"
      ];
      # See https://nixos.org/manual/nix/stable/command-ref/conf-file#conf-use-xdg-base-directories
      use-xdg-base-directories = true;

      # Add some "caches" (substituters)
      substituters = [
        "https://cache.nixos.org/"
        "https://r-ryantm.cachix.org"
        "https://shortbrain.cachix.org"
        "https://vdemeester.cachix.org"
        "https://chapeau-rouge.cachix.org"
      ];
      trusted-public-keys = [
        "r-ryantm.cachix.org-1:gkUbLkouDAyvBdpBX0JOdIiD2/DP1ldF3Z3Y6Gqcc4c="
        "shortbrain.cachix.org-1:dqXcXzM0yXs3eo9ChmMfmob93eemwNyhTx7wCR4IjeQ="
        "mic92.cachix.org-1:gi8IhgiT3CYZnJsaW7fxznzTkMUOn1RY4GmXdT/nXYQ="
        "chapeau-rouge.cachix.org-1:r34IG766Ez4Eeanr7Zx+egzXLE2Zgvc+XRspYZPDAn8="
        "vdemeester.cachix.org-1:eZWNOrLR9A9szeMahn9ENaoT9DB3WgOos8va+d2CU44="
      ];
    };

    extraOptions = ''
      connect-timeout = 20
      build-cores = 0
      keep-outputs = true
      keep-derivations = true
      builders-use-substitutes = true
    '';

    # On laptops at least, make the daemon and builders low priority
    # to have a responding system while building
    daemonIOSchedClass = "idle";
    daemonCPUSchedPolicy = "idle";
  };

  # `nix-daemon` will hit the stack limit when using `nixFlakes`.
  systemd.services.nix-daemon.serviceConfig."LimitSTACK" = "infinity";

  system = {
    inherit stateVersion;
  };

}
