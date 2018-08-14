{ stdenv, lib, fetchurl, patchelf, unzip, file }:

stdenv.mkDerivation rec {
  name = "op-${version}";
  version = "0.5.1";

  src = if stdenv.system == "i686-linux" then
    fetchurl {
      url = "https://cache.agilebits.com/dist/1P/op/pkg/v${version}/op_linux_386_v${version}.zip";
      sha256 = "011s77krmp2fj3889fyiwhx3f9854yykd1i34y9pa8gy2hgxkh85";
    }
  else if stdenv.system == "x86_64-linux" then
    fetchurl {
      url = "https://cache.agilebits.com/dist/1P/op/pkg/v${version}/op_linux_amd64_v${version}.zip";
      sha256 = "18yp1ridj9v9x54jyz8yyjvnhcv6gz5bjqjv4sl8dqiy6bli4wzv";
    }
  else throw "platform ${stdenv.system} not supported!";

  buildInputs = [ patchelf unzip file ];
  buildCommand = ''
    unpackFile $src
    mkdir -p $out/bin
    cp -a ./op $out/bin/op.wrapped
    chmod +x $out/bin/op.wrapped
    echo "#!/usr/bin/env bash" > $out/bin/op
    echo $(< $NIX_CC/nix-support/dynamic-linker) $out/bin/op.wrapped \"\$@\" >> $out/bin/op
    chmod +x $out/bin/op
  '';

  meta = {
    description = "1password CLI";
    homepage = "https://1password.com/";
    license = lib.licenses.unfree;
    platforms = lib.platforms.linux;
  };
}
