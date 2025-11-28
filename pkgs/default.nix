# Custom packages, that can be defined similarly to ones from nixpkgs
# Build them using 'nix build .#example' or (legacy) 'nix-build -A example'

{
  pkgs ? (import ../nixpkgs.nix) { },
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
  ape = pkgs.callPackage ./ape { };
  ram = pkgs.callPackage ./ram { };
  govanityurl = pkgs.callPackage ./govanityurl { };
  batzconverter = pkgs.callPackage ./batzconverter { };
  manifest-tool = pkgs.callPackage ./manifest-tool { };
  gh-restart-failed = pkgs.callPackage ../tools/gh-restart-failed { };
  arr = pkgs.callPackage ../tools/arr { };
  fedora-vm = pkgs.callPackage ../tools/fedora-vm { };

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
      sha256 = "09aprlawi3m7qrdas3zlk52niap2sr741qzfpjwc2c22hrlyv8ng";
    };
  };

}
