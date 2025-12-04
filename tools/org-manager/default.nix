{
  lib,
  buildGoModule,
}:

buildGoModule {
  pname = "org-manager";
  version = "0.1.0";

  src = ./.;

  # Build from cmd/org-manager subdirectory
  subPackages = [ "cmd/org-manager" ];

  vendorHash = "sha256-hocnLCzWN8srQcO3BMNkd2lt0m54Qe7sqAhUxVZlz1k=";

  ldflags = [
    "-s"
    "-w"
  ];

  meta = with lib; {
    description = "Tool for managing org-mode files including backup, validation, and link checking";
    longDescription = ''
      org-manager is a comprehensive tool for managing org-mode files with features including:
      - Backing up readwise and pkai notes
      - Validating org-mode file structure and metadata
      - Checking links (local, denote, id links)
      - Integration with go-org-readwise for syncing highlights
    '';
    homepage = "https://github.com/vdemeester/home";
    license = licenses.asl20;
    platforms = platforms.unix;
    mainProgram = "org-manager";
  };
}
