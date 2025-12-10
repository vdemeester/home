{
  stdenv,
  lib,
  makeWrapper,
  gh,
  fzf,
  jq,
  git,
  emacs,
}:

stdenv.mkDerivation {
  name = "gh-resolve-conflicts";
  pname = "gh-resolve-conflicts";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    cp gh-resolve-conflicts.sh $out/bin/gh-resolve-conflicts
    chmod +x $out/bin/gh-resolve-conflicts

    wrapProgram $out/bin/gh-resolve-conflicts \
      --prefix PATH : ${
        lib.makeBinPath [
          gh
          fzf
          jq
          git
          emacs
        ]
      }
  '';

  meta = {
    description = "List and resolve merge conflicts in GitHub pull requests interactively";
    platforms = lib.platforms.unix;
  };
}
