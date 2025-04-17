{ inputs, ... }:
{
  # FIXME: migrate to pkgs and overlays on root
  additions = final: _prev: import ../../pkgs { pkgs = final; };
  modifications = _final: prev: {
    # example = prev.example.overrideAttrs (oldAttrs: rec {
    # ...
    # });
    # custom-caddy = import ./custom-caddy.nix { pkgs = prev; };
  };

  # When applied, the unstable nixpkgs set (declared in the flake inputs) will
  # be accessible through 'pkgs.unstable'
  unstable-packages = final: _prev: {
    master = import inputs.nixpkgs-master {
      inherit (final) system;
      config.allowUnfree = true;
      overlays = [
        (_final: _prev: {
          # example = prev.example.overrideAttrs (oldAttrs: rec {
          # ...
          # });
        })
      ];
    };
    unstable = import inputs.nixpkgs {
      inherit (final) system;
      config.allowUnfree = true;
      overlays = [
        (_final: _prev: {
          # example = prev.example.overrideAttrs (oldAttrs: rec {
          # ...
          # });
        })
      ];
    };
  };
  tekton = final: prev: {

    inherit (prev.callPackage ../packages/tkn { })
      tkn_0_17
      tkn_0_18
      tkn_0_19
      tkn_0_20
      tkn_0_21
      tkn_0_22
      tkn_0_23
      tkn
      ;
    inherit (prev.callPackage ../packages/tkn/tkn-pac.nix { })
      tkn-pac_0_5
      tkn-pac_0_6
      tkn-pac_0_7
      tkn-pac_0_8
      tkn-pac
      ;
    inherit (prev.callPackage ../packages/tkn/tkn-local.nix { })
      tkn-local_0_4
      tkn-local_0_3
      tkn-local_0_2
      tkn-local
      ;
  };
}
