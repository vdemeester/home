{
  lib,
  stdenv,
  makeWrapper,
  git,
  nix,
  jq,
  curl,
}:

stdenv.mkDerivation {
  pname = "nix-flake-update";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp nix-flake-update.sh $out/bin/nix-flake-update
    chmod +x $out/bin/nix-flake-update

    wrapProgram $out/bin/nix-flake-update \
      --prefix PATH : ${
        lib.makeBinPath [
          git
          nix
          jq
          curl
        ]
      }

    runHook postInstall
  '';

  meta = with lib; {
    description = "Automated NixOS flake.lock updater with build verification";
    license = licenses.mit;
    platforms = platforms.linux;
  };
}
