# https://github.com/chmouel/chmouzies/tree/master/kubernetes
{
  stdenv,
  fetchFromGitLab,
  python313,
  installShellFiles,
}:

stdenv.mkDerivation rec {
  name = "chmouzies.kubernetes";

  src = fetchFromGitLab {
    owner = "chmouel";
    repo = "chmouzies";
    rev = "67191c029cb0d68e0cc2f384fabe6c62a0189e27";
    sha256 = "sha256-XJOms+r86Y2Y9UiWEP0IG9TqMsI6TgiNZrkDinHxOhc=";
  };

  propagatedBuildInputs = [ python313 ];
  nativeBuildInputs = [ installShellFiles ];

  builder = ./builder.kubernetes.sh;
}
