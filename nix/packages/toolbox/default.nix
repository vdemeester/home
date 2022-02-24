{ stdenv, lib, fetchFromGitHub }:

stdenv.mkDerivation rec {
  pname = "toolbox";
  version = "0.0.99.3";
  name = "${pname}-${version}";

  src = fetchFromGitHub {
    owner = "containers";
    repo = "toolbox";
    rev = "${version}";
    sha256 = "sha256-9HiWgEtaMypLOwXJ6Xg3grLSZOQ4NInZtcvLPV51YO8=";
  };

  phases = "unpackPhase installPhase";

  installPhase = ''
    mkdir $out
    install -D toolbox $out/bin/toolbox
    install -D profile.d/toolbox.sh $out/share/toolbox/profile.d/toolbox.sh
    sed -i 's%/etc/profile.d/%$out/share/toolbox/profile.d/%g' toolbox
    mkdir -p $out/share/bash-completion/completions/
    cp completion/bash/toolbox $out/share/bash-completion/completions/toolbox
  '';
}
