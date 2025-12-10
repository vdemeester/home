# Custom packages, that can be defined similarly to ones from nixpkgs
# Build them using 'nix build .#example' or (legacy) 'nix-build -A example'

{
  pkgs ? (import ../nixpkgs.nix) { },
  globals ? { },
}:
let
  compileEmacsFiles = pkgs.callPackage ./emacs/builder.nix;
in
{
  # TODO: migrate things from nix/packages
  nixfmt-plus = pkgs.callPackage ./nixfmt-plus.nix { };
  scripts = pkgs.callPackage ./my/scripts { };
  vrsync = pkgs.callPackage ./my/vrsync { };
  vde-thinkpad = pkgs.callPackage ./my/vde-thinkpad { };
  battery-monitor = pkgs.callPackage ../tools/battery-monitor { };
  claude-hooks = pkgs.callPackage ../tools/claude-hooks { };
  ape = pkgs.callPackage ./ape { };
  ram = pkgs.callPackage ./ram { };
  govanityurl = pkgs.callPackage ./govanityurl { };
  batzconverter = pkgs.callPackage ./batzconverter { };
  manifest-tool = pkgs.callPackage ./manifest-tool { };
  gh-restart-failed = pkgs.callPackage ../tools/gh-restart-failed { };
  arr = pkgs.callPackage ../tools/arr { };
  download-kiwix-zim = pkgs.callPackage ../tools/download-kiwix-zim { };
  toggle-color-scheme = pkgs.callPackage ./toggle-color-scheme { };
  homepage = pkgs.callPackage ./homepage { inherit globals; };

  chmouzies-ai = pkgs.callPackage ./chmouzies/ai.nix { };
  chmouzies-git = pkgs.callPackage ./chmouzies/git.nix { };
  chmouzies-kubernetes = pkgs.callPackage ./chmouzies/kubernetes.nix { };

  systemd-email = pkgs.callPackage ./systemd-email { };

  bookmark-plus = compileEmacsFiles {
    name = "bookmark-plus";
    src = pkgs.fetchFromGitHub {
      owner = "emacsmirror";
      repo = "bookmark-plus";
      rev = "73b8e1c2195860a8c24b5e2961914780c19cf5e3";
      hash = "sha256-z6LtaYZCMMG4vO7jQE7W4qpoRZn0D61axqeOyBXNVyU=";
    };
  };

}
