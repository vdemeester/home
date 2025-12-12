{
  buildGoModule,
  lib,
  makeWrapper,
  installShellFiles,
  gh,
  fzf,
  jq,
}:

buildGoModule {
  pname = "gh-pr";
  version = "0.1.0";
  src = ./.;

  vendorHash = "sha256-hocnLCzWN8srQcO3BMNkd2lt0m54Qe7sqAhUxVZlz1k="; # No external dependencies

  nativeBuildInputs = [
    makeWrapper
    installShellFiles
  ];

  # Build all binaries
  subPackages = [ "cmd/gh-pr" ];

  # Wrap binary to include gh, fzf, and jq in PATH and install completions
  postInstall = ''
    wrapProgram $out/bin/gh-pr \
      --prefix PATH : ${
        lib.makeBinPath [
          gh
          fzf
          jq
        ]
      }

    # Generate shell completions
    installShellCompletion --cmd gh-pr \
      --bash <($out/bin/gh-pr completion bash) \
      --fish <($out/bin/gh-pr completion fish) \
      --zsh <($out/bin/gh-pr completion zsh)
  '';

  meta = {
    description = "GitHub Pull Request management tool with template support, workflow restart, conflict resolution, and batch commenting";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
    mainProgram = "gh-pr";
  };
}
