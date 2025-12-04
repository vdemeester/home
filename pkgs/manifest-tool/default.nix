{
  lib,
  buildGoModule,
  fetchFromGitHub,
  installShellFiles,
}:

buildGoModule rec {
  name = "manifest-tool-${version}";
  pname = name;
  version = "2.2.1";
  rev = "v${version}";

  subPackages = [ "cmd/manifest-tool" ];
  modRoot = "./v2";

  src = fetchFromGitHub {
    inherit rev;
    owner = "estesp";
    repo = "manifest-tool";
    sha256 = "sha256-aw8c8VhSFexUpQqXDOd/pRSiuRl4njBe+LDONTVK7Uw=";
  };
  vendorHash = null;

  nativeBuildInputs = [ installShellFiles ];

  postInstall = ''
    # urfave/cli v2 shell completion support
    installShellCompletion --cmd manifest-tool \
      --bash <($out/bin/manifest-tool completion bash) \
      --fish <($out/bin/manifest-tool completion fish) \
      --zsh <($out/bin/manifest-tool completion zsh)
  '';

  meta = {
    description = "Tool for inspecting and creating multi-platform container image manifests";
    homepage = "https://github.com/estesp/manifest-tool";
    license = lib.licenses.asl20;
    maintainers = with lib.maintainers; [ vdemeester ];
  };

}
