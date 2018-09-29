self: super:
{
  ape = import ../pkgs/ape {
    inherit (self) stdenv lib buildGoPackage fetchFromGitHub;
  };
  dobi = import ../pkgs/dobi {
    inherit (self) stdenv lib buildGoPackage fetchFromGitHub;
  };
  dep-collector = import ../pkgs/dep-collector {
    inherit (self) stdenv lib fetchgit buildGoPackage;
  };
  protobuild = import ../pkgs/protobuild {
    inherit (self) stdenv lib buildGoPackage fetchgit;
  };
  go-containerregistry = import ../pkgs/go-containerregistry {
    inherit (self) stdenv lib buildGoPackage fetchgit;
  };
  gogo-protobuf = import ../pkgs/gogo-protobuf {
    inherit (self) stdenv lib buildGoPackage fetchgit;
  };
  kubespy = import ../pkgs/kubespy {
    inherit (self) stdenv lib buildGoPackage fetchgit;
  };
  scripts = import ../pkgs/scripts {
    inherit (self) stdenv;
  };
  envbox = import ../pkgs/envbox {
    inherit (self) stdenv lib buildGoPackage fetchFromGitHub;
  };
  prm = import ../pkgs/prm {
    inherit (self) stdenv lib buildGoPackage fetchgit;
  };
  tmux-tpm = import ../pkgs/tmux-tpm {
    inherit (self) stdenv lib fetchFromGitHub;
  };
  vscode-with-extensions = super.vscode-with-extensions.override {
      # code --list-extensions --show-versions
      # ls ~/.vscode/extensions
      # find version at https://marketplace.visualstudio.com/items?itemName=ms-python.python -> version
      vscodeExtensions =
        super.vscode-utils.extensionsFromVscodeMarketplace [
          {
            name = "EditorConfig";
            publisher = "EditorConfig";
            version = "0.12.4";
            sha256 = "067mxkzjmgz9lv5443ig7jc4dpgml4pz0dac0xmqrdmiwml6j4k4";
          }
          {
            name = "vsc-material-theme";
            publisher = "Equinusocio";
            version = "2.1.0";
            sha256 = "1rhygrig9z1kxy0ldw28zm6xldjji53s1apxl2n9yi9vl0wn8np1";
          }
          {
            name = "material-icon-theme";
            publisher = "PKief";
            version = "3.5.0";
            sha256 = "0djr8dxhpf81y3x2haj7jriqsmi87may43myph3rgqmzw2s986sg";
          }
          {
            name = "Nix";
            publisher = "bbenoist";
            version = "1.0.1";
            sha256 = "0zd0n9f5z1f0ckzfjr38xw2zzmcxg1gjrava7yahg5cvdcw6l35b";
          }
          {
            name = "fish-vscode";
            publisher = "skyapps";
            version = "0.2.0";
            sha256 = "148r186y3h7n84fcyh6wa2qwl2q3pfi8aykwkc9dhfj3kwfcm5rb";
          }
        ];
        # ] ++ with super.vscode-extensions; [
        #   bbenoist.Nix
        # ];
    };
}
