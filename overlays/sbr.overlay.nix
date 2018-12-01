self: super: {
  vrsync = import ../pkgs/vrsync {
    inherit (self) stdenv lib;
  };
  vde-thinkpad = import ../pkgs/vde-thinkpad {
    inherit (self) stdenv lib;
  };
}
