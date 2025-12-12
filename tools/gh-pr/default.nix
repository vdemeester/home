{
  buildGoModule,
  lib,
  makeWrapper,
  gh,
}:

buildGoModule {
  pname = "gh-pr";
  version = "0.1.0";
  src = ./.;

  vendorHash = "sha256-hocnLCzWN8srQcO3BMNkd2lt0m54Qe7sqAhUxVZlz1k="; # No external dependencies

  nativeBuildInputs = [ makeWrapper ];

  # Build all binaries
  subPackages = [ "cmd/gh-pr" ];

  # Wrap binary to include gh in PATH
  postInstall = ''
    wrapProgram $out/bin/gh-pr \
      --prefix PATH : ${lib.makeBinPath [ gh ]}
  '';

  meta = {
    description = "GitHub Pull Request management tool with template support, workflow restart, and conflict resolution";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
    mainProgram = "gh-pr";
  };
}
