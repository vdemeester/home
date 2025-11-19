{ ... }:
{
  imports = [
    ../common/hardware/acpid.nix
  ];

  hardware = {
    # opengl.extraPackages = with pkgs; [ vaapiIntel libvdpau-va-gl vaapiVdpau intel-ocl intel-media-driver ];
  };
}
