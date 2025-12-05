# https://github.com/chmouel/chmouzies/tree/master/kubernetes
{
  stdenv,
  lib,
  fetchFromGitLab,
  python313,
  installShellFiles,
}:

stdenv.mkDerivation {
  pname = "chmouzies-git";
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
        "gh-prcheck"
        "gh-actionfollow"
        "gh-clone"
        "gh-issuef"
        "gh-issuecreate"
        "gh-completer"
        "git-branch-cleanup"
        "git-remote-branch"
        "git-wt-delete"
        "git-wt-create"
      ];
    in
    ''
      runHook preInstall

      mkdir -p $out/bin
      ${lib.concatMapStringsSep "\n" (b: "cp $src/git/${b} $out/bin/") binaries}

      installShellCompletion --cmd gh-issuecreate --zsh $src/git/_gh-issuecreate
      installShellCompletion --cmd gh-clone --zsh $src/git/_gh-clone

      runHook postInstall
    '';

  meta = with lib; {
    description = "Chmouel's git-related helper scripts and GitHub CLI extensions";
    homepage = "https://gitlab.com/chmouel/chmouzies";
    platforms = platforms.unix;
  };
}
