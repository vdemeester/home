{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # containers
    oras
    skopeo
    rekor-cli
    cosign
    # kubernetes
    crane
    kail
    ko
    kubectl
    kubernetes-helm
    kustomize
    kss
    krew
    oc
    omc
    opm
    openshift-install
    operator-sdk
    snazy
    kubelogin-oidc
    my.chmouzies.kubernetes # FIXME update this
    tektoncd-cli
    # knd
    # build
    dagger
    # google
    (google-cloud-sdk.withExtraComponents (
      with google-cloud-sdk.components;
      [
        gke-gcloud-auth-plugin
        gcloud-man-pages
        cloud-run-proxy
        terraform-tools
      ]
    ))
  ];
}
