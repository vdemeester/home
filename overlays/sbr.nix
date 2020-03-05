self: super:
{
  scripts = import ../pkgs/scripts {
    inherit (self) stdenv;
  };
  tmux-tpm = import ../pkgs/tmux-tpm {
    inherit (self) stdenv lib fetchFromGitHub;
  };
  vscodeliveshare = import ../pkgs/vscodeliveshare {
    inherit (self) stdenv vscode-utils autoPatchelfHook xorg gnome3 utillinux openssl icu zlib curl lttng-ust libsecret libkrb5 gcc libunwind binutils;
  };
  emacs27 = (self.emacs.override { srcRepo = true; }).overrideAttrs(old: {
    name = "emacs-dev";
    version = "27.0.99";
    src = super.fetchFromGitHub {
      owner = "emacs-mirror";
      repo = "emacs";
      rev = "emacs-27.0.90";
      sha256 = "13n82lxbhmkcmlzbh0nml8ydxyfvz8g7wsdq7nszlwmq914gb5nk";
    };
    buildInputs = old.buildInputs ++ [ super.jansson ];
    patches = [
      ./patches/clean-env.patch
    ];
  });
}
