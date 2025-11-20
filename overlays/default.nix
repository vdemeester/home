{ inputs, ... }:
{
  # FIXME: migrate to pkgs and overlays on root
  additions = final: _prev: import ../pkgs { pkgs = final; };
  modifications = final: prev: {
    # example = prev.example.overrideAttrs (oldAttrs: rec {
    # ...
    # });
    # custom-caddy = import ./custom-caddy.nix { pkgs = prev; };
    go_1_25_3 = prev.go_1_25.overrideAttrs (_finalAttrs: {
      version = "1.25.3";
      src = final.fetchurl {
        url = "https://go.dev/dl/go1.25.3.src.tar.gz";
        hash = "sha256-qBpLpZPQAV4QxR4mfeP/B8eskU38oDfZUX0ClRcJd5U=";
      };
    });

    buildGo1253Module = prev.buildGoModule.override {
      go = final.go_1_25_3;
    };

    cosign = prev.cosign.override {
      buildGoModule = final.buildGo1253Module;
    };
  };

  # When applied, the unstable nixpkgs set (declared in the flake inputs) will
  # be accessible through 'pkgs.unstable'
  unstable-packages = final: _prev: {
    master = import inputs.nixpkgs-master {
      inherit (final) system;
      config.allowUnfree = true;
      overlays = [
        (_final: _prev: {
          claude-code = _prev.claude-code;
          claude-code-acp = _prev.claude-code-acp;
          gemini-cli = _prev.gemini-cli;
          cursor-cli = _prev.cursor-cli;
          code-cursor = _prev.code-cursor;
          antigravity = _prev.antigravity;
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
