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
    deployment.targetHost = "192.168.1.130";
    # deployment.targetHost = "k8sn1.home";
    deployment.tags = [ "kubernetes" "master" ];
    deployment.healthChecks = {
      cmd = [{
        cmd = [ "kubectl" "--kubeconfig=/etc/kubernetes/cluster-admin.kubeconfig" "version" ];
        description = "Validating that kubectl is available and a cluster is running";
      }];
    };
    imports = [ ../../systems/hosts/k8sn1.nix ];
  };
  "k8sn2" = { config, pkgs, lib, ... }: {
    deployment.targetUser = "root";
    deployment.targetHost = "192.168.1.131";
    # deployment.targetHost = "k8sn2.home";
    deployment.tags = [ "kubernetes" "worker" ];
    imports = [ ../../systems/hosts/k8sn2.nix ];
  };
  "k8sn3" = { config, pkgs, lib, ... }: {
    deployment.targetUser = "root";
    deployment.targetHost = "192.168.1.132";
    # deployment.targetHost = "k8sn3.home";
    deployment.tags = [ "kubernetes" "worker" ];
    imports = [ ../../systems/hosts/k8sn3.nix ];
  };
  "wakasu" = { config, pkgs, lib, ... }: {
    deployment.targetUser = "root";
    deployment.targetHost = "wakasu.home";
    deployment.tags = [ "baremetal" "wakasu" ];
    imports = [ ../../systems/hosts/wakasu.nix ];
  };
  "sakhalin" = { config, pkgs, lib, ... }: {
    deployment.targetUser = "root";
    deployment.targetHost = "sakhalin.home";
    deployment.tags = [ "baremetal" "sakhalin" ];
    imports = [ ../../systems/hosts/sakhalin.nix ];
  };
  "okinawa" = { config, pkgs, lib, ... }: {
    deployment.targetUser = "root";
    deployment.targetHost = "okinawa.home";
    deployment.tags = [ "baremetal" "okinawa" ];
    imports = [ ../../systems/hosts/okinawa.nix ];
  };
}
