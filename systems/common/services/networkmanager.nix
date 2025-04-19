{ config
, lib
, pkgs
, ...
}: {
  networking = {
    networkmanager = {
      enable = true;
      wifi = {
        backend = "iwd";
      };
      unmanaged = [
        "interface-name:br-*"
        "interface-name:ve-*" # FIXME are those docker's or libvirt's
        "interface-name:veth-*" # FIXME are those docker's or libvirt's
      ]
      # Do not manager wireguard
      ++ lib.optionals config.networking.wireguard.enable [ "interface-name:wg0" ]
      # Do not manage docker interfaces
      ++ lib.optionals config.virtualisation.docker.enable [ "interface-name:docker0" ]
      # Do not manager libvirt interfaces
      ++ lib.optionals config.virtualisation.libvirtd.enable [ "interface-name:virbr*" ];
      plugins = with pkgs; [ networkmanager-openvpn ];
    };
  };

  # Workaround https://github.com/NixOS/nixpkgs/issues/180175
  systemd.services.NetworkManager-wait-online.enable = false;
}
