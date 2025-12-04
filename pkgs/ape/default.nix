{
  lib,
  buildGoModule,
  fetchgit,
  installShellFiles,
}:

buildGoModule rec {
  name = "ape-${version}";
  version = "0.4.2";
  rev = "v${version}";

  src = fetchgit {
    inherit rev;
    url = "https://git.sr.ht/~vdemeester/ape";
    sha256 = "sha256-Ww2HuwR5Fx/tnYHARqeuDv2NU26oQPyIjtkYj291WTg=";
  };
  vendorHash = "sha256-XLRjxJu28yt02+SykO8OqvFCl0nSFyVbHKRUGcS7Mrs=";

  nativeBuildInputs = [ installShellFiles ];

  postInstall = ''
    installShellCompletion --cmd ape \
      --bash <($out/bin/ape completion bash) \
      --fish <($out/bin/ape completion fish) \
      --zsh <($out/bin/ape completion zsh)
  '';

  meta = {
    description = "a git mirror *upstream* updater ";
    homepage = "https://git.sr.ht/~vdemeester/ape";
    license = lib.licenses.asl20;
  };
}
