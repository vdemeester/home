# Secrets Workflow

Manage secrets with agenix for NixOS and home-manager.

## When to Use

- "manage secrets with nix"
- "agenix secrets"
- "encrypt secrets nixos"
- "age encryption"

## Quick Commands

### Basic Operations
```bash
# Edit secret
agenix -e secrets/mySecret.age

# Re-key all secrets (after adding keys)
agenix -r

# Re-key specific secret
agenix -e secrets/mySecret.age -r
```

### In Configuration
```nix
# Use secret in NixOS
age.secrets.mySecret = {
  file = ../secrets/mySecret.age;
  owner = "myuser";
  group = "mygroup";
};

# Reference secret path
services.myservice.passwordFile = config.age.secrets.mySecret.path;
```

## Setup agenix

### Install agenix
```nix
# In flake.nix inputs
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, agenix }: {
    nixosConfigurations.hostname = nixpkgs.lib.nixosSystem {
      modules = [
        agenix.nixosModules.default
        ./configuration.nix
      ];
    };
  };
}
```

### Install agenix CLI
```bash
# Install globally
nix profile install github:ryantm/agenix

# Or in dev shell
nix-shell -p agenix
```

## Creating Secrets

### Define secrets.nix
```nix
# secrets/secrets.nix
let
  # User SSH keys (for editing secrets)
  user1 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIUser1PublicKey user1@example.com";
  user2 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIUser2PublicKey user2@example.com";

  # System SSH keys (for decrypting on host)
  system1 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAISystem1PublicKey root@system1";
  system2 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAISystem2PublicKey root@system2";

  # Group keys together
  users = [ user1 user2 ];
  systems = [ system1 system2 ];
  all = users ++ systems;
in
{
  # Secret accessible by all
  "database-password.age".publicKeys = all;

  # Secret only for system1
  "system1-api-key.age".publicKeys = [ user1 system1 ];

  # Secret for specific systems
  "shared-secret.age".publicKeys = users ++ [ system1 system2 ];
}
```

### Get SSH Public Keys
```bash
# User SSH key
cat ~/.ssh/id_ed25519.pub

# System SSH key
cat /etc/ssh/ssh_host_ed25519_key.pub

# Or from known_hosts
ssh-keyscan hostname

# Convert to age format (if needed)
ssh-keygen -y -f ~/.ssh/id_ed25519 | ssh-to-age
```

## Encrypt and Edit Secrets

### Create New Secret
```bash
# Create and edit secret
agenix -e secrets/mySecret.age

# Opens $EDITOR with decrypted content
# Save and exit to encrypt
```

### Edit Existing Secret
```bash
# Edit secret
agenix -e secrets/database-password.age

# Update password in editor
# Save and exit to re-encrypt
```

### Non-Interactive Encryption
```bash
# Encrypt from file
echo "super-secret-value" | agenix -e secrets/mySecret.age

# Or pipe content
cat plain-secret.txt | agenix -e secrets/mySecret.age
```

## Using Secrets in Configuration

### NixOS Configuration
```nix
{ config, pkgs, ... }:

{
  # Import agenix module
  imports = [ agenix.nixosModules.default ];

  # Define secret
  age.secrets.database-password = {
    file = ../secrets/database-password.age;
    owner = "postgres";
    group = "postgres";
    mode = "0440";
  };

  # Use secret in service
  services.postgresql = {
    enable = true;
    # Secret path available at runtime
    # Don't use in Nix expressions directly!
  };

  # Use in systemd service
  systemd.services.myservice = {
    serviceConfig = {
      EnvironmentFile = config.age.secrets.database-password.path;
    };
  };
}
```

### Home-Manager Configuration
```nix
{ config, pkgs, ... }:

{
  imports = [ agenix.homeManagerModules.default ];

  age.secrets.github-token = {
    file = ../secrets/github-token.age;
    path = "${config.home.homeDirectory}/.config/github/token";
  };

  # Use in programs
  programs.git = {
    extraConfig = {
      credential.helper = "store --file=${config.age.secrets.github-token.path}";
    };
  };
}
```

### Secret Options
```nix
age.secrets.mySecret = {
  # Path to encrypted secret file
  file = ../secrets/mySecret.age;

  # Where to place decrypted secret (default: /run/agenix/<name>)
  path = "/var/lib/myapp/secret";

  # Ownership
  owner = "myuser";
  group = "mygroup";

  # Permissions (octal)
  mode = "0440";

  # Symlink instead of copy
  symlink = true;

  # Name in /run/agenix/ (default: attribute name)
  name = "custom-name";
};
```

## Common Secret Patterns

### Database Credentials
```nix
age.secrets.postgres-password = {
  file = ../secrets/postgres-password.age;
  owner = "postgres";
  mode = "0440";
};

services.postgresql = {
  enable = true;
  authentication = ''
    # Use password from secret
  '';
};

# Create database user with secret password
systemd.services.postgres-init = {
  after = [ "postgresql.service" ];
  wantedBy = [ "multi-user.target" ];
  script = ''
    ${pkgs.postgresql}/bin/psql -c \
      "ALTER USER myuser PASSWORD '$(cat ${config.age.secrets.postgres-password.path})';"
  '';
};
```

### API Keys
```nix
age.secrets.api-key = {
  file = ../secrets/api-key.age;
  owner = "myservice";
};

systemd.services.myservice = {
  script = ''
    export API_KEY=$(cat ${config.age.secrets.api-key.path})
    ${pkgs.myservice}/bin/myservice
  '';
};
```

### SSH Keys
```nix
age.secrets.deploy-key = {
  file = ../secrets/deploy-key.age;
  owner = "git";
  mode = "0600";
  path = "/home/git/.ssh/deploy_key";
};
```

### Certificates
```nix
age.secrets.tls-cert = {
  file = ../secrets/tls-cert.age;
  owner = "nginx";
  group = "nginx";
  mode = "0440";
};

age.secrets.tls-key = {
  file = ../secrets/tls-key.age;
  owner = "nginx";
  group = "nginx";
  mode = "0400";
};

services.nginx.virtualHosts."example.com" = {
  forceSSL = true;
  sslCertificate = config.age.secrets.tls-cert.path;
  sslCertificateKey = config.age.secrets.tls-key.path;
};
```

## Re-keying Secrets

### Add New Host
```nix
# 1. Get new host SSH key
ssh-keyscan new-host

# 2. Add to secrets.nix
let
  newHost = "ssh-ed25519 AAAAC3... root@new-host";
in
{
  "mySecret.age".publicKeys = [ user1 system1 newHost ];
}
```

```bash
# 3. Re-key all secrets
agenix -r

# 4. Commit changes
git add secrets/
git commit -m "chore: re-key secrets for new-host"
```

### Rotate User Keys
```bash
# 1. Update secrets.nix with new keys

# 2. Re-key all secrets
agenix -r

# 3. Old keys can no longer decrypt
# Ensure all secrets are updated before removing old keys!
```

### Emergency Re-key
```bash
# If key is compromised
# 1. Remove compromised key from secrets.nix
# 2. Re-key all secrets immediately
agenix -r

# 3. Deploy to all systems
# 4. Verify secrets still work
# 5. Rotate actual secret values
```

## Yubikey Integration

### Setup Yubikey for Secrets
```bash
# Generate age key from yubikey
age-plugin-yubikey --generate

# Get public key
age-plugin-yubikey --list
```

### Use Yubikey in secrets.nix
```nix
let
  yubikey1 = "age1yubikey1...";
  yubikey2 = "age1yubikey1...";
in
{
  "mySecret.age".publicKeys = [ yubikey1 yubikey2 ];
}
```

## Best Practices

1. **Never commit plaintext secrets**: Always encrypt before committing
2. **Use specific keys**: Only give access to those who need it
3. **Separate secrets by environment**: Different secrets for dev/prod
4. **Rotate secrets regularly**: Especially after team changes
5. **Backup encryption keys**: Store SSH keys securely
6. **Test secret decryption**: Verify after re-keying
7. **Use git-crypt for repositories**: Additional layer for entire repo
8. **Document secret purpose**: Comment what each secret is for
9. **Monitor secret access**: Log when secrets are accessed
10. **Plan for key compromise**: Know your re-key procedure

## Troubleshooting

### Cannot Decrypt Secret
```
error: Failed to decrypt
```

**Check:**
1. Is host SSH key in secrets.nix?
2. Does /etc/ssh/ssh_host_ed25519_key exist?
3. Was secret encrypted with correct keys?

```bash
# Verify host key matches
cat /etc/ssh/ssh_host_ed25519_key.pub

# Re-key if needed
agenix -r
```

### Secret File Not Found
```
error: file 'secrets/mySecret.age' does not exist
```

**Fix:**
```bash
# Create the secret
agenix -e secrets/mySecret.age

# Or check path is correct
ls secrets/
```

### Permission Denied
```
error: cannot create symlink
```

**Fix:**
```nix
# Ensure parent directory exists
systemd.tmpfiles.rules = [
  "d /var/lib/myapp 0755 myuser mygroup -"
];

age.secrets.mySecret = {
  file = ../secrets/mySecret.age;
  path = "/var/lib/myapp/secret";
  owner = "myuser";
  group = "mygroup";
};
```

## Advanced Patterns

### Multi-Environment Secrets
```nix
# secrets/secrets.nix
let
  prodSystems = [ prod1 prod2 ];
  devSystems = [ dev1 dev2 ];
in
{
  "prod-db-password.age".publicKeys = users ++ prodSystems;
  "dev-db-password.age".publicKeys = users ++ devSystems;
}
```

### Conditional Secrets
```nix
{ config, lib, ... }:

{
  age.secrets = lib.mkIf config.services.database.enable {
    db-password = {
      file = ../secrets/db-password.age;
      owner = config.services.database.user;
    };
  };
}
```

### Templated Secrets
```nix
# Generate config from secret
systemd.services.myservice = {
  preStart = ''
    cat > /etc/myservice/config.json <<EOF
    {
      "api_key": "$(cat ${config.age.secrets.api-key.path})",
      "database_url": "postgresql://user:$(cat ${config.age.secrets.db-password.path})@localhost/db"
    }
    EOF
  '';
};
```

## Migration to agenix

### From Plain Files
```bash
# 1. Create secrets.nix

# 2. Encrypt existing secrets
for file in /etc/secrets/*; do
  secret_name=$(basename "$file")
  agenix -e "secrets/${secret_name}.age" < "$file"
done

# 3. Update configuration to use agenix

# 4. Remove plain files after verification
```

### From sops-nix
```bash
# 1. Decrypt sops secrets
sops -d secrets.yaml > plaintext.yaml

# 2. Split into individual files
# Extract each secret

# 3. Encrypt with agenix
for secret in secrets/*; do
  agenix -e "$secret.age" < "$secret"
done

# 4. Update configuration
```

## Resources

- [agenix Documentation](https://github.com/ryantm/agenix)
- [age Encryption](https://age-encryption.org/)
- [age-plugin-yubikey](https://github.com/str4d/age-plugin-yubikey)
- [NixOS Secrets Management](https://nixos.wiki/wiki/Comparison_of_secret_managing_schemes)
