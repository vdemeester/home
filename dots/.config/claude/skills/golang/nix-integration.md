# Go and Nix Integration

This document provides guidance when working with Go projects that use Nix for building and packaging.

## When to Use This Document

This integration guide applies when:
- You detect `flake.nix`, `default.nix`, or `shell.nix` in the project root
- The project is in a Nix-managed repository (contains `/pkgs` or `/tools` with Nix definitions)
- The user explicitly mentions Nix or NixOS

## Project Structure for Nix-Based Go Projects

### Typical Layout in ~/src/home Repository
```
tools/<tool-name>/
├── cmd/
│   └── <tool-name>/
│       └── main.go          # Entry point
├── internal/                # Private application code
│   ├── config/
│   ├── handlers/
│   └── models/
├── pkg/                     # Public library code (if needed)
├── go.mod                   # Module definition
├── go.sum                   # Dependency checksums
├── README.md
└── *_test.go               # Tests alongside code

pkgs/<tool-name>/
└── default.nix             # Nix package definition
```

### Multi-Architecture Support
- Build for x86_64-linux and aarch64-linux
- Nix handles cross-compilation automatically
- Custom packages exposed via overlays in `/overlays`

## Building Go Tools with Nix

### Build Commands
```bash
# Build a Go tool package
nix build .#<package-name>

# Install to profile
nix profile install .#<package-name>

# Check what would be built
nix build .#<package-name> --dry-run

# Build for specific architecture
nix build .#<package-name> --system aarch64-linux
```

### Development Shell
```bash
# Enter dev shell with Go tools available
nix develop

# Run tests in development shell
cd tools/<tool-name>
go test ./...

# Format code (pre-commit hooks use gofmt)
gofmt -w .
```

## Package Definition with buildGoModule

### Basic Package Definition
```nix
# In pkgs/<package-name>/default.nix
{ lib, buildGoModule }:

buildGoModule {
  pname = "tool-name";
  version = "1.0.0";

  # Source from tools directory
  src = ../../tools/tool-name;

  # Vendor hash for dependency management
  # Set to lib.fakeHash initially, then update with actual hash
  vendorHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";

  # Build flags (optional)
  ldflags = [
    "-s"
    "-w"
    "-X main.version=${version}"
  ];

  # Metadata
  meta = with lib; {
    description = "Tool description";
    homepage = "https://github.com/user/repo";
    license = licenses.mit;
    maintainers = [ ];
  };
}
```

### Updating Vendor Hash
```bash
# Initial build to get vendor hash
nix build .#<package-name> 2>&1 | grep "got:"

# Copy the hash from the error message to vendorHash in default.nix
```

### Common buildGoModule Options
```nix
buildGoModule {
  # ... other options ...

  # Exclude certain packages from building
  excludedPackages = [ "cmd/debug-tool" ];

  # Skip tests during build
  doCheck = false;

  # Additional build inputs
  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ sqlite ];

  # Post-install steps
  postInstall = ''
    installShellCompletion --cmd tool-name \
      --bash <($out/bin/tool-name completion bash) \
      --zsh <($out/bin/tool-name completion zsh)
  '';

  # CGO settings
  CGO_ENABLED = 0;  # Static binary
}
```

## Testing in Nix Environment

### Running Tests
```bash
# From repository root
nix develop
cd tools/<tool-name>
go test ./...

# With coverage
go test -cover ./...

# Verbose output
go test -v ./...
```

### Pre-commit Hooks
The repository uses pre-commit hooks for Go:
- `gofmt` for formatting
- Can be installed with `make install-hooks`

```bash
# Install hooks
make install-hooks

# Run pre-commit checks manually
make pre-commit
```

## Integration with Repository Build System

### Makefile Targets (if available)
```bash
# Build Go tools via Nix
make build

# Run tests
make test

# Format code
make fmt

# Clean build artifacts
make clean
```

## Dependency Management with Nix

### Adding Dependencies
```bash
# Add Go dependency normally
cd tools/<tool-name>
go get github.com/pkg/errors
go mod tidy

# Update Nix package vendorHash
nix build .#<package-name>  # Will show new hash
```

### Updating Dependencies
```bash
# Update Go modules
cd tools/<tool-name>
go get -u ./...
go mod tidy

# Update vendorHash in pkgs/<package-name>/default.nix
# Rebuild to verify
nix build .#<package-name>
```

## Common Patterns

### Tool with Subcommands
```nix
buildGoModule {
  pname = "multi-tool";
  version = "1.0.0";
  src = ../../tools/multi-tool;

  # Build only the main command
  subPackages = [ "cmd/multi-tool" ];

  vendorHash = "sha256-...";
}
```

### Tool with Static Assets
```nix
buildGoModule {
  pname = "web-tool";
  version = "1.0.0";
  src = ../../tools/web-tool;

  vendorHash = "sha256-...";

  # Embed static assets
  preBuild = ''
    cp -r ${../../tools/web-tool/assets} ./assets
  '';
}
```

### Tool Requiring External Dependencies
```nix
{ lib, buildGoModule, pkg-config, sqlite }:

buildGoModule {
  pname = "db-tool";
  version = "1.0.0";
  src = ../../tools/db-tool;

  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ sqlite ];

  vendorHash = "sha256-...";

  # Enable CGO for database drivers
  CGO_ENABLED = 1;
}
```

## Troubleshooting

### Vendor Hash Mismatch
```bash
# Problem: vendorHash error during build
# Solution: Update hash with the one shown in error message
nix build .#<package-name> 2>&1 | grep "got:"
```

### Missing Dependencies
```bash
# Problem: Missing system libraries
# Solution: Add to buildInputs or nativeBuildInputs
buildInputs = [ pkg-config sqlite ];
```

### Test Failures in Nix Build
```bash
# Problem: Tests fail during nix build but pass with `go test`
# Solution: Disable tests in package definition
doCheck = false;

# Or fix test dependencies
checkInputs = [ git ];
```

## Best Practices for Nix + Go

1. **Keep go.mod/go.sum updated**: Always run `go mod tidy` after dependency changes
2. **Update vendorHash promptly**: Don't commit package definitions with incorrect hashes
3. **Test in nix develop**: Ensure tools work in Nix environment before building package
4. **Use proper source paths**: Reference tool sources with relative paths (e.g., `../../tools/name`)
5. **Document build requirements**: Note any special build flags or dependencies in README
6. **Leverage overlays**: Add custom packages to overlays for repository-wide availability
7. **Version consistently**: Keep version in sync between go.mod and default.nix

## Resources

- Nix Manual: https://nixos.org/manual/nix/stable/
- buildGoModule documentation: https://nixos.org/manual/nixpkgs/stable/#sec-language-go
- Repository structure: See `/pkgs` and `/tools` directories for examples
