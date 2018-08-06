{ stdenv, lib, fetchFromGitHub }:

stdenv.mkDerivation rec {
  name = "tmux-tpm";
  rev = "95f78336c3972f3e6648b7b3db754f2224320a5e";
  version = "20170902-${lib.strings.substring 0 7 rev}";

  src = fetchFromGitHub {
    inherit rev;
    owner = "tmux-plugins";
    repo = "tpm";
    sha256 = "01jjbvzrjqlxxdgszkrnx4rapcqh1sh7k7y6mk42ymrzfkrbi0rd";
  };

  builder = ./builder.sh;

  meta = {
    description = "Tmux Plugin Manager";
    homepage = "https://github.com/tmux-plugins/tpm";
    license = lib.licenses.mit;
  };
}
