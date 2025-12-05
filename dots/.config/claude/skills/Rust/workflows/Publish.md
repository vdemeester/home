# Publish Workflow

Publish Rust crates to crates.io.

## When to Use

- "publish crate"
- "cargo publish"
- "crates.io"

## Preparation

### 1. Get API Token

```bash
# Login to crates.io
cargo login

# Or set token manually
cargo login <your-api-token>
```

### 2. Configure Cargo.toml

```toml
[package]
name = "my-awesome-crate"
version = "0.1.0"
edition = "2021"
authors = ["Your Name <you@example.com>"]
description = "A short description of your crate"
documentation = "https://docs.rs/my-awesome-crate"
repository = "https://github.com/user/my-awesome-crate"
license = "MIT OR Apache-2.0"
keywords = ["cli", "tool", "utility"]  # Max 5
categories = ["command-line-utilities"]
readme = "README.md"

# Important: exclude large or unnecessary files
exclude = [
    "/.github",
    "/target",
    "*.png",
    "benches/",
]
```

## Publishing

```bash
# Dry run (test package)
cargo publish --dry-run

# Package and inspect
cargo package --list

# Publish
cargo publish

# Publish with custom registry
cargo publish --registry my-registry
```

## Version Management

```bash
# Update version in Cargo.toml
# Use semantic versioning: MAJOR.MINOR.PATCH

# Tag release
git tag v0.1.0
git push --tags
```

## Semantic Versioning

- **MAJOR**: Breaking changes
- **MINOR**: New features (backward compatible)
- **PATCH**: Bug fixes (backward compatible)

```
0.1.0 -> 0.1.1  (bug fix)
0.1.1 -> 0.2.0  (new feature)
0.2.0 -> 1.0.0  (breaking change)
```

## Yanking Versions

```bash
# Yank a version (prevent new dependents)
cargo yank --vers 0.1.0

# Un-yank
cargo yank --vers 0.1.0 --undo
```

## Pre-Publish Checklist

- [ ] Update version in Cargo.toml
- [ ] Update CHANGELOG.md
- [ ] Run `cargo test --all-features`
- [ ] Run `cargo clippy --all-targets`
- [ ] Run `cargo doc --no-deps`
- [ ] Verify README.md is up to date
- [ ] Check license files present
- [ ] Run `cargo publish --dry-run`
- [ ] Tag release in git
- [ ] Publish to crates.io

## Best Practices

1. **Follow semver**: Strict semantic versioning
2. **Good documentation**: README and rustdoc
3. **Comprehensive tests**: All features tested
4. **Clear license**: MIT OR Apache-2.0 is common
5. **Meaningful keywords**: Help discoverability
6. **Keep it small**: Exclude unnecessary files
