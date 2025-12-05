# Dependencies Workflow

Manage Rust dependencies with Cargo.

## When to Use

- "add dependency"
- "cargo add"
- "update dependencies"

## Quick Commands

```bash
# Add dependency
cargo add serde

# Add with features
cargo add tokio --features full

# Add dev dependency
cargo add --dev proptest

# Add build dependency
cargo add --build cc

# Remove dependency
cargo rm serde

# Update dependencies
cargo update

# Update specific dependency
cargo update -p serde

# Show dependency tree
cargo tree

# Check for outdated dependencies
cargo outdated
```

## Cargo.toml

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
tokio = { version = "1.0", default-features = false, features = ["rt-multi-thread"] }
anyhow = "1.0"

# Git dependency
mylib = { git = "https://github.com/user/mylib" }

# Path dependency
utils = { path = "../utils" }

# Optional dependency
encryption = { version = "1.0", optional = true }

[dev-dependencies]
criterion = "0.7"

[build-dependencies]
cc = "1.0"

[features]
default = ["std"]
std = []
secure = ["encryption"]
```

## Workspace Dependencies

```toml
# Root Cargo.toml
[workspace.dependencies]
serde = { version = "1.0", features = ["derive"] }
tokio = "1.0"

# Member Cargo.toml
[dependencies]
serde = { workspace = true }
tokio = { workspace = true, features = ["full"] }
```

## Version Requirements

```toml
[dependencies]
exact = "=1.2.3"          # Exactly 1.2.3
caret = "^1.2.3"          # >=1.2.3, <2.0.0 (default)
tilde = "~1.2.3"          # >=1.2.3, <1.3.0
wildcard = "1.*"          # >=1.0.0, <2.0.0
range = ">=1.2, <1.5"     # Range
```

## Best Practices

1. **Lock files**: Commit for apps, ignore for libraries
2. **Minimal versions**: Use lowest compatible version
3. **Feature gates**: Only enable needed features
4. **Workspace deps**: Share versions across workspace
5. **Regular updates**: Check for security updates
6. **Audit dependencies**: `cargo audit`
