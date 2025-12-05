# Build Workflow

Build Rust projects with Cargo using various profiles and configurations.

## When to Use

- "build rust project"
- "cargo build"
- "compile rust"
- "release build"

## Quick Commands

```bash
# Debug build (fast compilation, with debug symbols)
cargo build

# Release build (optimized, slower compilation)
cargo build --release

# Check compilation without building
cargo check

# Build all targets (bins, libs, tests, benches, examples)
cargo build --all-targets

# Build with all features enabled
cargo build --all-features

# Build specific binary
cargo build --bin mybinary

# Build for specific target
cargo build --target x86_64-unknown-linux-musl

# Cross-compilation
cargo build --target aarch64-unknown-linux-gnu
```

## Build Profiles

### Development Profile

```toml
# Cargo.toml
[profile.dev]
opt-level = 0        # No optimization
debug = true         # Full debug info
split-debuginfo = "unpacked"  # Fast debug builds
incremental = true   # Incremental compilation
```

### Release Profile

```toml
[profile.release]
opt-level = 3        # Maximum optimization
lto = true           # Link-time optimization
codegen-units = 1    # Better optimization (slower compile)
strip = true         # Remove debug symbols
panic = "abort"      # Smaller binary size
```

### Custom Profiles

```toml
# Fast development builds
[profile.dev-opt]
inherits = "dev"
opt-level = 1        # Some optimization

# Smaller release builds
[profile.release-small]
inherits = "release"
opt-level = "z"      # Optimize for size
lto = true
strip = true
```

```bash
# Use custom profile
cargo build --profile dev-opt
```

## Cross-Compilation

### Install Target

```bash
# List installed targets
rustup target list --installed

# Add target
rustup target add x86_64-unknown-linux-musl
rustup target add aarch64-unknown-linux-gnu
rustup target add wasm32-unknown-unknown
```

### Build for Target

```bash
# Build for target
cargo build --target x86_64-unknown-linux-musl --release

# Configure target in .cargo/config.toml
# .cargo/config.toml
[build]
target = "x86_64-unknown-linux-musl"

[target.x86_64-unknown-linux-musl]
linker = "x86_64-linux-musl-gcc"
```

## Build Features

### Conditional Compilation

```toml
# Cargo.toml
[features]
default = ["std"]
std = []
encryption = ["aes", "sha2"]
network = ["reqwest", "tokio"]

[dependencies]
aes = { version = "0.8", optional = true }
reqwest = { version = "0.11", optional = true }
```

```bash
# Build with specific features
cargo build --features encryption

# Build with multiple features
cargo build --features "encryption,network"

# Build without default features
cargo build --no-default-features

# Build with all features
cargo build --all-features
```

## Build Scripts

### build.rs

```rust
// build.rs - runs before compilation
use std::env;

fn main() {
    // Set environment variables
    println!("cargo:rustc-env=BUILD_TIME={}", chrono::Utc::now());

    // Link to system library
    println!("cargo:rustc-link-lib=ssl");

    // Rerun if file changes
    println!("cargo:rerun-if-changed=src/config.rs");

    // Conditional compilation
    if cfg!(target_os = "linux") {
        println!("cargo:rustc-cfg=linux_specific");
    }
}
```

## Optimization Tips

### 1. Use `cargo check` for Development

```bash
# Faster than build, only checks for errors
cargo check

# With all features
cargo check --all-features
```

### 2. Parallel Builds

```bash
# Use more CPU cores (default is number of CPUs)
cargo build -j 8
```

### 3. Incremental Compilation

```toml
# Cargo.toml
[profile.dev]
incremental = true  # Enabled by default for dev
```

### 4. Shared Build Cache

```bash
# Use sccache for caching
export RUSTC_WRAPPER=sccache
cargo build
```

## Build Artifacts

### Location

```
target/
├── debug/           # Debug builds
│   ├── mybinary    # Executable
│   └── libmylib.rlib
├── release/         # Release builds
│   └── mybinary
└── CACHEDIR.TAG
```

### Clean Build

```bash
# Remove all build artifacts
cargo clean

# Remove only release artifacts
cargo clean --release

# Remove specific package artifacts
cargo clean -p mypackage
```

## Build Times

### Measure Build Times

```bash
# Time a build
time cargo build

# Detailed timing info
cargo build --timings

# Opens HTML report
cargo build --timings --open
```

### Speed Up Builds

1. **Use `cargo check` during development**
2. **Enable incremental compilation**
3. **Reduce codegen-units in dev profile**
4. **Use `lld` linker** (faster than default)

```toml
# .cargo/config.toml
[target.x86_64-unknown-linux-gnu]
linker = "clang"
rustflags = ["-C", "link-arg=-fuse-ld=lld"]
```

## Workspace Builds

```bash
# Build all workspace members
cargo build --workspace

# Build specific package in workspace
cargo build -p mypackage

# Build all except specific package
cargo build --workspace --exclude mypackage
```

## Best Practices

1. **Use `cargo check` frequently** - Faster than build
2. **Profile appropriately** - Debug for dev, release for production
3. **Enable LTO for releases** - Smaller, faster binaries
4. **Strip symbols in release** - Reduce binary size
5. **Use build features** - Conditional compilation
6. **Cache dependencies** - Use sccache or similar
7. **Measure build times** - Use `--timings` to identify bottlenecks
