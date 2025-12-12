{ buildGoModule }:

buildGoModule {
  pname = "cliphist-cleanup";
  version = "0.1.0";
  src = ./.;

  vendorHash = null;

  meta = {
    description = "Clean up cliphist clipboard history by pattern matching";
    mainProgram = "cliphist-cleanup";
  };
}
