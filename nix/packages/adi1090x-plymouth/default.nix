{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  pname = "adi1090x-plymouth";
  version = "0.0.1";

  src = builtins.fetchGit {
    url = "https://github.com/adi1090x/plymouth-themes";
  };

  buildInputs = [
    pkgs.git
  ];

  configurePhase = ''
    mkdir -p $out/share/plymouth/themes/
  '';

  buildPhase = ''
  '';

  installPhase = ''
    cp -r pack_1/* $out/share/plymouth/themes
    cp -r pack_2/* $out/share/plymouth/themes
    cp -r pack_3/* $out/share/plymouth/themes
    cp -r pack_4/* $out/share/plymouth/themes
    for p in $out/share/plymouth/themes/*; do
      theme=$(basename $p)
      sed -i "s@\/usr\/@$out\/@" $out/share/plymouth/themes/$theme/$theme.plymouth
    done
  '';
}
