_: {
  boot = {
    binfmt.emulatedSystems = [
      "armv6l-linux"
      "armv7l-linux"
      "aarch64-linux"
      "riscv32-linux"
      "riscv64-linux"
    ];

    # On most distros, people use https://github.com/multiarch/qemu-user-static or
    # https://github.com/tonistiigi/binfmt or https://github.com/dbhi/qus to setup
    # binfmt_misc registrations with their kernel. This strategy works because in
    # --privileged mode, docker containers can access the host filesystem via mounts.
    # They ship with static builds of qemu-user, mount /proc/sys/fs/binfmt_misc,
    # add registrations to it, and exit. Those binfmt_misc registrations have the F
    # flag, so the kernel allocates file descriptors for the qemu binaries
    # immediately upon registration. Now, when containers are created and the
    # kernel comes across non-native binaries inside the chroot, instead of doing a
    # path lookup for the qemu binary (which would obviously fail unless the qemu
    # binary is added to the container manually), it simply uses the already-opened
    # file descriptor for it. This requires the qemu binaries to be fully static, as
    # any dynamic library lookups will obviously fail within the chroot/container.
    # This article by the author of the binfmt_misc F flag explains everything really
    # well: https://lwn.net/Articles/679308/
    # Also see this StackOverflow answer: https://stackoverflow.com/a/72890225/11424968
    binfmt.preferStaticEmulators = true;
  };
}
