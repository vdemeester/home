{ pkgs, ... }:

{
  xdg.configFile."mr".source = ./mr/lib.mr;
  home.file."src/.mrconfig".source = ./mr/src.mr;
  home.file."src/tektoncd".source = ./mr/src.tektoncd.mr;
  home.file."src/go.sbr.pm/.mrconfig".source = ./mr/src.go.sbr.pm.mr;
  home.file."src/k8s.io/.mrconfig".source = ./mr/src.k8s.io.mr;
  home.file."src/knative.dev/.mrconfig".source = ./mr/src.knative.dev.mr;
  home.file."src/github.com/.mrconfig".source = ./mr/src.github.mr;
  home.file."src/github.com/openshift/.mrconfig".source = ./mr/src.github.openshift.mr;
  home.file."src/github.com/openshift-pipelines/.mrconfig".source = ./mr/src.github.openshift-pipelines.mr;
  home.file."src/pkgs.devel.redhat.com/.mrconfig".source = ./mr/src.pkgs.devel.redhat.mr;
}
