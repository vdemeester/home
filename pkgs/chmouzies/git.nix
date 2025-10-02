# https://github.com/chmouel/chmouzies/tree/master/kubernetes
{
  stdenv,
  fetchFromGitLab,
  python313,
  installShellFiles,
}:

stdenv.mkDerivation rec {
  name = "chmouzies-git";
  pname = "chmouzies-git";

  version = "0-unstable-2025-09-30";

  src = fetchFromGitLab {
    owner = "chmouel";
    repo = "chmouzies";
    rev = "10773a982503829e5f276a3bd8fd526dab4f92d3";
    sha256 = "sha256-Lr4tYzgEhvVhJhSRDGuUlrp7XP2iUNX7H2nizlEko3Q=";
  };

  propagatedBuildInputs = [ python313 ];
  nativeBuildInputs = [ installShellFiles ];

  builder = ./builder.git.sh;
}
