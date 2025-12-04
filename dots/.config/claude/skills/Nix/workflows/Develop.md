# Develop Workflow

Create and use Nix development environments with direnv integration.

## When to Use

- "nix development shell"
- "nix develop"
- "development environment"
- "direnv with nix"

## Quick Commands

### Development Shells
```bash
# Enter development shell
nix develop

# Enter shell for specific package
nix develop .#package-name

# Run command in shell
nix develop -c make build
nix develop --command npm test

# Print shell environment
nix develop --print-build-logs
```

### direnv Integration
```bash
# Create .envrc
echo "use flake" > .envrc

# Allow direnv
direnv allow

# Reload environment
direnv reload

# Check status
direnv status
```

## Creating Development Shells

### Basic devShell
```nix
# flake.nix
{
  outputs = { self, nixpkgs }: {
    devShells.x86_64-linux.default = let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in pkgs.mkShell {
      buildInputs = with pkgs; [
        go
        gopls
        golangci-lint
      ];

      shellHook = ''
        echo "Welcome to dev environment"
        go version
      '';
    };
  };
}
```

### Multi-Shell Setup
```nix
# Multiple development shells
{
  outputs = { self, nixpkgs }: {
    devShells.x86_64-linux = {
      default = pkgs.mkShell {
        buildInputs = [ pkgs.go ];
      };

      rust = pkgs.mkShell {
        buildInputs = with pkgs; [
          rustc
          cargo
          rust-analyzer
        ];
      };

      python = pkgs.mkShell {
        buildInputs = with pkgs; [
          python3
          python3Packages.pip
        ];
      };
    };
  };
}
```

### Access Specific Shell
```bash
# Use default shell
nix develop

# Use named shell
nix develop .#rust
nix develop .#python
```

## Shell Configuration

### Environment Variables
```nix
pkgs.mkShell {
  buildInputs = [ pkgs.go ];

  shellHook = ''
    export GOPATH=$PWD/.go
    export GOCACHE=$PWD/.cache/go-build
    export PATH=$GOPATH/bin:$PATH

    # Create directories
    mkdir -p $GOPATH $GOCACHE
  '';

  # Or use env
  env = {
    GOPATH = "$PWD/.go";
    GOCACHE = "$PWD/.cache/go-build";
  };
}
```

### Shell Hooks
```nix
pkgs.mkShell {
  buildInputs = [ pkgs.nodejs ];

  shellHook = ''
    # Print banner
    echo "================================"
    echo "Node.js Development Environment"
    echo "================================"
    node --version
    npm --version

    # Install dependencies
    if [ ! -d "node_modules" ]; then
      npm install
    fi

    # Setup git hooks
    if [ ! -f ".git/hooks/pre-commit" ]; then
      echo "Setting up git hooks..."
      npx husky install
    fi
  '';
}
```

### Package-Specific Shell
```nix
# For a specific package's dev environment
{
  outputs = { self, nixpkgs }: {
    packages.x86_64-linux.myapp = /* package definition */;

    devShells.x86_64-linux.default = let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in pkgs.mkShell {
      # Include all build inputs from package
      inputsFrom = [ self.packages.x86_64-linux.myapp ];

      # Add development tools
      buildInputs = with pkgs; [
        go-tools
        delve
        gopls
      ];
    };
  };
}
```

## direnv Integration

### Setup direnv
```bash
# Install direnv
nix profile install nixpkgs#direnv
nix profile install nixpkgs#nix-direnv

# Configure shell (bash)
eval "$(direnv hook bash)"

# Configure shell (zsh)
eval "$(direnv hook zsh)"
```

### Basic .envrc
```bash
# .envrc
use flake

# Optional: Use specific flake output
# use flake .#rust
```

### Advanced .envrc
```bash
# .envrc
use flake

# Watch additional files
watch_file shell.nix
watch_file flake.nix
watch_file flake.lock

# Set environment variables
export EDITOR=vim
export DATABASE_URL=postgresql://localhost/mydb

# Source local secrets
if [ -f .env.local ]; then
  dotenv .env.local
fi

# Layout for different languages
# layout python  # Auto-create venv
# layout node    # Add node_modules/.bin to PATH
```

### nix-direnv Configuration
```bash
# ~/.config/direnv/direnvrc or ~/.direnvrc
source $HOME/.nix-profile/share/nix-direnv/direnvrc

# Or if installed via home-manager
# It's automatically configured
```

### Performance with nix-direnv
```bash
# nix-direnv caches the environment
# Much faster reloads!

# First load (slow)
$ cd project/
direnv: loading ~/project/.envrc
direnv: using flake
# ... downloads and builds ...

# Subsequent loads (instant!)
$ cd project/
direnv: loading ~/project/.envrc
direnv: using cached flake environment
```

## Common Development Patterns

### Go Development
```nix
{
  devShells.x86_64-linux.default = pkgs.mkShell {
    buildInputs = with pkgs; [
      go
      gopls
      gotools
      golangci-lint
      delve
    ];

    shellHook = ''
      export GOPATH=$PWD/.go
      export GOCACHE=$PWD/.cache/go-build
      export PATH=$GOPATH/bin:$PATH
      mkdir -p $GOPATH $GOCACHE
    '';
  };
}
```

### Rust Development
```nix
{
  devShells.x86_64-linux.default = pkgs.mkShell {
    buildInputs = with pkgs; [
      rustc
      cargo
      rustfmt
      rust-analyzer
      clippy
    ];

    RUST_SRC_PATH = "${pkgs.rust.packages.stable.rustPlatform.rustLibSrc}";
  };
}
```

### Node.js Development
```nix
{
  devShells.x86_64-linux.default = pkgs.mkShell {
    buildInputs = with pkgs; [
      nodejs_20
      nodePackages.npm
      nodePackages.typescript
      nodePackages.prettier
    ];

    shellHook = ''
      export NODE_PATH=$PWD/node_modules
      export PATH=$PWD/node_modules/.bin:$PATH
    '';
  };
}
```

### Python Development
```nix
{
  devShells.x86_64-linux.default = let
    python = pkgs.python3.withPackages (ps: with ps; [
      flask
      requests
      pytest
    ]);
  in pkgs.mkShell {
    buildInputs = [ python ];

    shellHook = ''
      # Create virtual environment if needed
      if [ ! -d "venv" ]; then
        python -m venv venv
      fi
      source venv/bin/activate
    '';
  };
}
```

### C/C++ Development
```nix
{
  devShells.x86_64-linux.default = pkgs.mkShell {
    buildInputs = with pkgs; [
      gcc
      cmake
      gnumake
      gdb
      clang-tools
    ];

    shellHook = ''
      export CC=gcc
      export CXX=g++
    '';
  };
}
```

## Advanced Shell Features

### Multiple Package Sets
```nix
{
  devShells.x86_64-linux.default = let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
    unstable = inputs.nixpkgs-unstable.legacyPackages.x86_64-linux;
  in pkgs.mkShell {
    buildInputs = [
      pkgs.go             # Stable Go
      unstable.gopls      # Latest gopls
    ];
  };
}
```

### Conditional Tools
```nix
pkgs.mkShell {
  buildInputs = with pkgs; [
    go
    gopls
  ] ++ lib.optionals stdenv.isLinux [
    # Linux-specific tools
    strace
    ltrace
  ] ++ lib.optionals stdenv.isDarwin [
    # macOS-specific tools
    darwin.apple_sdk.frameworks.Security
  ];
}
```

### Project-Specific Scripts
```nix
pkgs.mkShell {
  buildInputs = [ pkgs.go ];

  shellHook = ''
    # Create helper scripts
    cat > run.sh <<'EOF'
    #!/usr/bin/env bash
    go run ./cmd/server
    EOF
    chmod +x run.sh

    cat > test.sh <<'EOF'
    #!/usr/bin/env bash
    go test ./... -v
    EOF
    chmod +x test.sh

    echo "Helper scripts created: run.sh, test.sh"
  '';
}
```

## Development Workflow

### Typical Session
```bash
# Enter project
cd myproject/

# direnv automatically loads environment
direnv: loading ~/myproject/.envrc
direnv: using flake

# Environment ready
go version
gopls version

# Work on project
vim main.go
go build
go test

# Leave project
cd ..

# Environment unloaded
direnv: unloading
```

### Update Development Environment
```bash
# Update flake inputs
nix flake update

# Reload direnv
direnv reload

# Or manually re-enter
nix develop
```

### Share Environment
```bash
# Commit flake.nix and flake.lock
git add flake.nix flake.lock
git commit -m "chore: add nix development environment"

# Team members get same environment
git clone repo
cd repo
direnv allow  # Or nix develop
```

## Troubleshooting

### direnv Not Loading
```bash
# Check direnv status
direnv status

# Manually allow
direnv allow

# Check .envrc syntax
cat .envrc

# Test flake
nix flake check
```

### Slow Shell Activation
```bash
# Install nix-direnv for caching
nix profile install nixpkgs#nix-direnv

# Configure in ~/.config/direnv/direnvrc
source $HOME/.nix-profile/share/nix-direnv/direnvrc

# Reload
direnv reload
```

### Environment Variables Not Set
```bash
# Check environment
nix develop --command printenv

# Verify shellHook
nix develop --command bash -c 'echo $MY_VAR'

# Debug direnv
direnv exec . env | grep MY_VAR
```

### Binary Cache Issues
```bash
# Use substituters
nix develop --extra-substituters https://cache.nixos.org

# Build locally if needed
nix develop --no-substitute
```

## Best Practices

1. **Use direnv**: Automatic environment loading
2. **Install nix-direnv**: Much faster reloads with caching
3. **Commit flake.lock**: Ensure reproducibility
4. **Pin versions**: Use specific nixpkgs inputs
5. **Minimal dependencies**: Only what's needed for development
6. **Document setup**: Add README with instructions
7. **Use inputsFrom**: Inherit from package definitions
8. **Test environment**: Verify with fresh clone

## Integration with Editors

### VS Code
```json
// .vscode/settings.json
{
  "nix.enableLanguageServer": true,
  "nix.serverPath": "nixd"
}
```

```bash
# Install nixd language server in devShell
buildInputs = [ pkgs.nixd ];
```

### Vim/Neovim
```bash
# Install LSPs in devShell
buildInputs = with pkgs; [
  gopls        # Go
  rust-analyzer # Rust
  nodePackages.typescript-language-server # TypeScript
];
```

### Emacs
```elisp
;; direnv integration
(use-package direnv
  :config
  (direnv-mode))
```

## Common Patterns

### Database Development
```nix
pkgs.mkShell {
  buildInputs = with pkgs; [
    postgresql
    pgcli
  ];

  shellHook = ''
    export PGDATA=$PWD/.postgres
    export PGHOST=$PWD/.postgres
    export PGDATABASE=mydb

    if [ ! -d "$PGDATA" ]; then
      initdb --no-locale --encoding=UTF8
      echo "unix_socket_directories = '$PGHOST'" >> $PGDATA/postgresql.conf
      pg_ctl start -l $PGDATA/logfile
      createdb $PGDATABASE
    else
      pg_ctl start -l $PGDATA/logfile
    fi
  '';
}
```

### Docker Development
```nix
pkgs.mkShell {
  buildInputs = with pkgs; [
    docker
    docker-compose
  ];

  shellHook = ''
    export DOCKER_HOST=unix://$XDG_RUNTIME_DIR/docker.sock
  '';
}
```

### Multiple Environments
```bash
# .envrc
# Choose environment based on directory
if [ -f "Cargo.toml" ]; then
  use flake .#rust
elif [ -f "go.mod" ]; then
  use flake .#go
elif [ -f "package.json" ]; then
  use flake .#node
else
  use flake
fi
```

## Resources

- [nix develop Manual](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-develop.html)
- [direnv Documentation](https://direnv.net/)
- [nix-direnv](https://github.com/nix-community/nix-direnv)
- [Development Environments](https://nixos.wiki/wiki/Development_environment_with_nix-shell)
