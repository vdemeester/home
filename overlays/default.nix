{ inputs, ... }:
{
  # FIXME: migrate to pkgs and overlays on root
  additions = final: _prev: import ../pkgs { pkgs = final; };
  modifications = _final: _prev: {
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

}
