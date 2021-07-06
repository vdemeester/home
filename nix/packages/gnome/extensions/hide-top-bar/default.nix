{ stdenv, fetchFromGitHub, glib, gettext, bash, nodePackages, gnome3, zip }:

stdenv.mkDerivation rec {
  pname = "hidetopbar";
  version = "2020-09-21";

  src = fetchFromGitHub {
    owner = "mlutfy";
    repo = "hidetopbar";
    rev = "994937d37528aa25bac954e4f1607adc8d991f4c";
    sha256 = "0cc4b3iid0fxrsrdwc9m1rnia7z9whbgdwkag0x1frlif4rknjrr";
  };

  nativeBuildInputs = [
    glib
    gettext
    nodePackages.typescript
    zip
  ];

  uuid = "hidetopbar@mathieu.bidon.ca";

  installPhase = ''
    mkdir -p $out/share/gnome-shell/extensions/${uuid}
    cp -r * $out/share/gnome-shell/extensions/${uuid}
  '';

  meta = with lib; {
    description = "Hide GNOME's top bar when it gets into your way.";
    license = licenses.gpl3;
    maintainers = with maintainers; [ vdemeester ];
    homepage = "https://github.com/mlutfy/hidetopbar";
  };
}
