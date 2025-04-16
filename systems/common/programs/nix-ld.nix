{ lib
, pkgs
, desktop
, ...
}: {
  programs = {
    nix-ld = {
      enable = true;
      # put whatever libraries you think you might need
      # nix-ld includes a strong sane-default as well
      # in addition to these
      libraries = with pkgs; [
        acl
        alsa-lib
        at-spi2-atk
        at-spi2-core
        atk
        attr
        bzip2
        cairo
        curl
        dbus
        expat
        fontconfig
        freetype
        fuse3
        gdk-pixbuf
        glib
        glibc
        icu
        libdrm
        libglvnd
        libnotify
        libpulseaudio
        libsecret
        libsodium
        libssh
        libunwind
        libusb1
        libuuid
        libxkbcommon
        mesa
        nspr
        nss
        openssl
        pango
        pipewire
        systemd
        stdenv.cc.cc # .lib
        util-linux
        zlib
        zstd
      ] ++ lib.optionals (builtins.isString desktop) [
        gtk3
        libGL
        libappindicator-gtk3
        vulkan-loader
        xorg.libX11
        xorg.libXScrnSaver
        xorg.libXcomposite
        xorg.libXcursor
        xorg.libXdamage
        xorg.libXext
        xorg.libXfixes
        xorg.libXi
        xorg.libXrandr
        xorg.libXrender
        xorg.libXtst
        xorg.libxcb
        xorg.libxkbfile
        xorg.libxshmfence
      ];
    };
  };
}
