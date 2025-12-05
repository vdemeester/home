# Workspace Workflow

Manage multi-crate projects with Cargo workspaces.

## When to Use

- "cargo workspace"
- "multi-crate project"
- "monorepo"

## Structure

```
myworkspace/
├── Cargo.toml              # Workspace root
├── Cargo.lock              # Shared lock file
├── crates/
│   ├── core/
│   │   ├── Cargo.toml
│   │   └── src/lib.rs
│   ├── cli/
│   │   ├── Cargo.toml
│   │   └── src/main.rs
│   └── api/
│       ├── Cargo.toml
│       └── src/lib.rs
```

## Root Cargo.toml

```toml
[workspace]
members = [
    "crates/*",
]
exclude = ["crates/experimental"]

# Use Rust 2024 resolver
resolver = "3"

# Shared dependencies
[workspace.dependencies]
serde = { version = "1.0", features = ["derive"] }
tokio = { version = "1.0", features = ["full"] }
anyhow = "1.0"

# Shared metadata
[workspace.package]
version = "0.1.0"
edition = "2021"
license = "MIT OR Apache-2.0"
authors = ["Your Name <you@example.com>"]

# Shared lints
[workspace.lints.rust]
unsafe_code = "forbid"

[workspace.lints.clippy]
pedantic = "warn"
```

## Member Cargo.toml

```toml
[package]
name = "myworkspace-cli"
version.workspace = true
edition.workspace = true
license.workspace = true

[dependencies]
# Workspace dependencies
serde.workspace = true
tokio.workspace = true

# Internal dependencies
myworkspace-core = { path = "../core" }

# Member-specific dependencies
clap = "4.0"
```

## Commands

```bash
# Build entire workspace
cargo build --workspace

# Build specific package
cargo build -p myworkspace-cli

# Test all packages
cargo test --workspace

# Test specific package
cargo test -p myworkspace-core

# Run binary from package
cargo run -p myworkspace-cli

# Check all packages
cargo check --workspace
```

## Best Practices

1. **Consistent naming**: `workspace-package` pattern
2. **Shared dependencies**: Use `workspace.dependencies`
3. **Version sync**: Use `version.workspace = true`
4. **One lock file**: Shared across workspace
5. **Logical separation**: Each crate has clear purpose
6. **Avoid circular deps**: Keep dependency graph acyclic
