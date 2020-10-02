{ stdenv, fetchFromGitLab, glib, gettext, bash, nodePackages, gnome3, unzip }:

stdenv.mkDerivation rec {
  pname = "nightthemeswitcher-gnome-shell-extension";
  version = "v39";

  src = fetchFromGitLab {
    owner = "rmnvgr";
    repo = "nightthemeswitcher-gnome-shell-extension";
    rev = "${version}";
    sha256 = "0lciranfqvp14vr1jh9syj0vx2vpiy2zww0m4pw9pay1nis37izf";
  };

  nativeBuildInputs = [
    glib
    gettext
    nodePackages.typescript
    gnome3.gnome-shell
    unzip
  ];

  uuid = "nightthemeswitcher@romainvigier.fr";

  installPhase = ''
    mkdir -p $out/share/gnome-shell/extensions
    unzip build/${uuid}.shell-extension.zip -d $out/share/gnome-shell/extensions/${uuid}
  '';

  meta = with stdenv.lib; {
    description = "Automatically toggle your light and dark GTK, GNOME Shell, icon and cursor themes variants, switch backgrounds and run custom commands at sunset and sunrise.";
    license = licenses.gpl3;
    maintainers = with maintainers; [ vdemeester ];
    homepage = "https://github.com/rmnvgr/nightthemeswitcher-gnome-shell-extension";
  };
}
