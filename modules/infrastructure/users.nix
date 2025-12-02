{ lib, ... }:
{
  options.infrastructure.users = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule {
        options = {
          sshKeys = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ ];
            description = "SSH public keys for this user";
          };
        };
      }
    );
    default = {
      vincent = {
        sshKeys = [
          # Yubikeys
          "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBFT5Rx+4Wuvd8lMBkcHxb4oHdRhm/OTg+p5tvPzoIN9enSmgRw5Inm/SlS8ZzV87G1NESTgzDRi6hREvqDlKvxs="
          "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBGHMa4rHuBbQQYv+8jvlkFCD2VYRGA4+5fnZAhLx8iDirzfEPqHB60UJWcDeixnJCUlpJjzFbS4crNOXhfCTCTE="
          "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBBFzxC16VqwTgWDQfw2YCiOw2JzpH3z9XgHtKoHhBdHi2i9m9XUc7fIUeEIIf7P8ARRNd8q5bjvl8JY7LtPkNCU="
          # AOMI (only "trusted" one)
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILJmTdMKYdgqpbQWBif58VBuwX+GqMGsMfB1ey1TKrM3 vincent@aomi"
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGThdcaPfIaB7d+K5uODqEusLKGI5ZCye0aNOCaMoInO Kyushu's ssh key"
        ];
      };
    };
    description = "User configurations";
  };
}
