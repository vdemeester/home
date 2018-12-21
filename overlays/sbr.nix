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
}
