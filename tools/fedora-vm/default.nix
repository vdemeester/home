{
  lib,
  stdenv,
  makeWrapper,
  curl,
  qemu,
  libvirt,
  cdrtools,
  openssh,
}:

stdenv.mkDerivation {
  pname = "fedora-vm";
  version = "dev";

  src = ./.;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    cp fedora-vm $out/bin/fedora-vm
    chmod +x $out/bin/fedora-vm

    wrapProgram $out/bin/fedora-vm \
      --prefix PATH : ${
        lib.makeBinPath [
          curl
          qemu
          libvirt
          cdrtools
          openssh
        ]
      }
  '';

  meta = with lib; {
    description = "Create and provision Fedora VMs with libvirt and cloud-init";
    longDescription = ''
      fedora-vm automates the creation of Fedora-based virtual machines using
      libvirt/virt-install with cloud-init for automatic provisioning. It can
      download Fedora cloud images, create VMs, wait for them to be ready,
      and execute scripts inside them.
    '';
    platforms = platforms.linux;
  };
}
