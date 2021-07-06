{ stdenv, fetchFromGitHub, glib, gettext, bash, nodePackages, gnome3 }:

stdenv.mkDerivation rec {
  pname = "gnome-bluetooth-quick-connect";
  version = "2020-09-03";

  src = fetchFromGitHub {
    owner = "bjarosze";
    repo = "gnome-bluetooth-quick-connect";
    rev = "662250e1ef3ebaafdb237c06ea39fbdbee09ee40";
    sha256 = "12lra0wkjdjm9bfjvan1hsfn55x0j2bsfgdjwqklggvv52iwr9wm";
  };

  nativeBuildInputs = [
    glib
    gettext
    nodePackages.typescript
  ];

  uuid = "bluetooth-quick-connect@bjarosze.gmail.com";

  installPhase = ''
    mkdir -p $out/share/gnome-shell/extensions/${uuid}
    cp -r * $out/share/gnome-shell/extensions/${uuid}
  '';

  meta = with lib; {
    description = "Allows paired Bluetooth devices to be connected and disconnected via the GNOME system menu, without need to enter the Settings app every time.";
    license = licenses.gpl3;
    maintainers = with maintainers; [ vdemeester ];
    homepage = "https://github.com/bjarosze/gnome-bluetooth-quick-connect";
  };
}
