# https://github.com/chmouel/chmouzies/tree/master/kubernetes
{
  stdenv,
  lib,
  fetchFromGitLab,
  python313,
  installShellFiles,
}:

stdenv.mkDerivation {
  pname = "chmouzies-kubernetes";
  version = "0-unstable-2025-09-30";

  src = fetchFromGitLab {
    owner = "chmouel";
    repo = "chmouzies";
    rev = "10773a982503829e5f276a3bd8fd526dab4f92d3";
    hash = "sha256-Lr4tYzgEhvVhJhSRDGuUlrp7XP2iUNX7H2nizlEko3Q=";
  };

  propagatedBuildInputs = [ python313 ];
  nativeBuildInputs = [ installShellFiles ];

  installPhase =
    let
      binaries = [
        "ocla"
        "kcl"
        "kdd"
        "kdp"
        "kselect"
        "kubectl-get-secret"
      ];
    in
    ''
      runHook preInstall

      mkdir -p $out/bin
      ${lib.concatMapStringsSep "\n" (b: "cp $src/kubernetes/${b} $out/bin/") binaries}

      installShellCompletion --cmd kcl --zsh $src/kubernetes/_kcl
      installShellCompletion --cmd kdd --zsh $src/kubernetes/_kdd
      installShellCompletion --cmd kdp --zsh $src/kubernetes/_kdp
      installShellCompletion --cmd kselect --zsh $src/kubernetes/_kselect
      installShellCompletion --cmd ocla --zsh $src/kubernetes/_ocla

      runHook postInstall
    '';

  meta = {
    description = "Chmouel's Kubernetes and OpenShift helper scripts";
    homepage = "https://gitlab.com/chmouel/chmouzies";
    platforms = lib.platforms.unix;
  };
}
