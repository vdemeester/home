{
  stdenv,
  lib,
  makeWrapper,
  gh,
  fzf,
  jq,
}:

stdenv.mkDerivation {
  name = "gh-restart-failed";
  pname = "gh-restart-failed";
  version = "0.1.0";

  src = ../../tools;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    cp gh-restart-failed.sh $out/bin/gh-restart-failed
    chmod +x $out/bin/gh-restart-failed

    wrapProgram $out/bin/gh-restart-failed \
      --prefix PATH : ${
        lib.makeBinPath [
          gh
          fzf
          jq
        ]
      }
  '';

  meta = {
    description = "List and restart failed GitHub workflow checks on pull requests";
    platforms = lib.platforms.unix;
  };
}
