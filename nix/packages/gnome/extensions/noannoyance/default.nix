{ stdenv, fetchFromGitHub, glib, gettext, bash, nodePackages, gnome3 }:

stdenv.mkDerivation rec {
  pname = "noannoyance";
  version = "2020-05-17";

  src = fetchFromGitHub {
    owner = "sindex";
    repo = "no-annoyance";
    rev = "9cc6aaf85cce8e434e58a9caa1b8abb077fdf2e3";
    sha256 = "1p0y871616rvd3wfw12nn8ybc8z76fcb7gcd13zf1q53wnwk5ynr";
  };

  nativeBuildInputs = [
    glib
    gettext
    nodePackages.typescript
  ];

  uuid = "noannoyance@sindex.com";

  installPhase = ''
    mkdir -p $out/share/gnome-shell/extensions/${uuid}
    cp -r * $out/share/gnome-shell/extensions/${uuid}
  '';

  meta = with lib; {
    description = "Disables the “Window Is Ready” notification and changes the policy of the window manager so that new windows are always focused.";
    license = licenses.gpl3;
    maintainers = with maintainers; [ vdemeester ];
    homepage = "https://github.com/sindex/no-annoyance";
  };
}
