# https://github.com/chmouel/chmouzies/tree/master/kubernetes
{
  stdenv,
  lib,
  fetchFromGitLab,
  python313,
  installShellFiles,
}:

stdenv.mkDerivation {
  pname = "chmouzies-ai";
  version = "0-unstable-2025-09-30";

  src = fetchFromGitLab {
    owner = "chmouel";
    repo = "chmouzies";
    rev = "10773a982503829e5f276a3bd8fd526dab4f92d3";
    sha256 = "sha256-Lr4tYzgEhvVhJhSRDGuUlrp7XP2iUNX7H2nizlEko3Q=";
  };

  propagatedBuildInputs = [ python313 ];
  nativeBuildInputs = [ installShellFiles ];

  installPhase =
    let
      binaries = [
        "aicommit"
        "git-commit-suggest-label"
      ];
    in
    ''
      runHook preInstall

      mkdir -p $out/bin
      ${lib.concatMapStringsSep "\n" (b: "cp $src/ai/${b} $out/bin/") binaries}

      installShellCompletion --cmd aicommit --zsh $src/ai/_aicommit

      runHook postInstall
    '';

  meta = with lib; {
    description = "Chmouel's AI-assisted git tools";
    homepage = "https://gitlab.com/chmouel/chmouzies";
    platforms = platforms.unix;
  };
}
