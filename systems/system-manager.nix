{
  hostname,
  ...
}:
{
  imports = [
    (./. + "/${hostname}/system.nix")

    # ./common/base
  ];

  nixpkgs = {
    # Note: Overlays cause infinite recursion in system-manager
    # Disabling for now - packages will come from standard nixpkgs
    # overlays = [
    #   # Our own flake exports (from overlays and pkgs dir)
    #   outputs.overlays.additions
    #   outputs.overlays.modifications
    #   outputs.overlays.unstable-packages
    #
    #   # And from other flakes
    #   inputs.emacs-overlay.overlay
    #   inputs.chapeau-rouge.overlays.openshift
    #   inputs.chick-group.overlays.default
    #   inputs.agenix.overlays.default
    #
    #   # Migrate to "modifications"
    #   (_: prev: {
    #     inherit (inputs.buildkit-tekton.packages.${prev.system}) tkn-local;
    #     inherit (inputs.dagger.packages.${prev.system}) dagger;
    #   })
    # ];
    config = {
      allowUnfree = true;
    };
  };
  nix = {
    # This will add each flake input as a registry
    # To make nix3 commands consistent with your flake
    # Note: These options are not available in system-manager
    # registry = lib.mkForce (lib.mapAttrs (_: value: { flake = value; }) inputs);

    # This will additionally add your inputs to the system's legacy channels
    # Making legacy nix commands consistent as well, awesome!
    # nixPath = lib.mkForce (
    #   lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry
    # );

    # Note: These options are not available in system-manager
    # optimise = {
    #   automatic = true;
    #   dates = [
    #     "01:10"
    #     "12:10"
    #   ];
    # };

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
    # Note: These options are not available in system-manager
    # daemonIOSchedClass = "idle";
    # daemonCPUSchedPolicy = "idle";
  };
}
