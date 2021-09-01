let
  sources = import ../../nix;
  pkgs = sources.pkgs { };
in
{
  network = {
    inherit pkgs;
    description = "Home network";
  };

  "k8sn1" = { config, pkgs, lib, ... }: {
    deployment.targetUser = "root";
    deployment.targetHost = "k8sn1.home";
    imports = [ ../../systems/hosts/k8sn1.nix ];
  };
  "k8sn2" = { config, pkgs, lib, ... }: {
    deployment.targetUser = "root";
    deployment.targetHost = "k8sn2.home";
    imports = [ ../../systems/hosts/k8sn2.nix ];
  };
  "k8sn3" = { config, pkgs, lib, ... }: {
    deployment.targetUser = "root";
    deployment.targetHost = "k8sn3.home";
    imports = [ ../../systems/hosts/k8sn3.nix ];
  };
  "wakasu" = { config, pkgs, lib, ... }: {
    deployment.targetUser = "root";
    deployment.targetHost = "wakasu.home";
    imports = [ ../../systems/hosts/wakasu.nix ];
  };
  "sakhalin" = { config, pkgs, lib, ... }: {
    deployment.targetUser = "root";
    deployment.targetHost = "sakhalin.home";
    imports = [ ../../systems/hosts/sakhalin.nix ];
  };
  "okinawa" = { config, pkgs, lib, ... }: {
    deployment.targetUser = "root";
    deployment.targetHost = "okinawa.home";
    imports = [ ../../systems/hosts/okinawa.nix ];
  };
  "hokkaido" = { config, pkgs, lib, ... }: {
    deployment.targetUser = "root";
    deployment.targetHost = "hokkaido.home";
    imports = [ ../../systems/hosts/hokkaido.nix ];
  };
}
