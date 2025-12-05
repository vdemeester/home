# Rust Skill

Modern Rust development guidance using Cargo, Clippy, and best practices.

## Overview

This skill provides comprehensive guidance for Rust development following 2025 best practices:

- **Cargo**: Build system and package manager
- **Clippy**: Over 550 lints for catching mistakes
- **rustfmt**: Official code formatter
- **Criterion**: Statistical benchmarking
- **thiserror/anyhow**: Modern error handling

## Workflows

### Building & Testing
- **[Build](workflows/Build.md)**: Compile projects with various profiles and targets
- **[Test](workflows/Test.md)**: Unit, integration, and property-based testing
- **[Bench](workflows/Bench.md)**: Performance benchmarking with Criterion

### Code Quality
- **[Lint](workflows/Lint.md)**: Lint and format with Clippy and rustfmt
- **[Error](workflows/Error.md)**: Idiomatic error handling patterns

### Dependencies & Publishing
- **[Deps](workflows/Deps.md)**: Manage dependencies with Cargo
- **[Workspace](workflows/Workspace.md)**: Multi-crate monorepos
- **[Publish](workflows/Publish.md)**: Publish crates to crates.io

## Quick Start

### New Project
```bash
# Create binary
cargo new myapp

# Create library
cargo new --lib mylib

cd myapp
cargo build
cargo run
```

### Add Dependencies
```bash
cargo add serde tokio anyhow
cargo add --dev proptest criterion
```

### Quality Checks
```bash
# Format code
cargo fmt

# Lint code
cargo clippy --all-targets --all-features -- -D warnings

# Run tests
cargo test

# Run benchmarks
cargo bench
```

## Key Concepts

### Ownership & Borrowing
Rust's ownership system ensures memory safety without garbage collection:
```rust
fn main() {
    let s = String::from("hello"); // s owns the string
    takes_ownership(s);             // s moved, no longer valid
    // println!("{}", s);           // Would error!

    let x = 5;
    makes_copy(x);                  // i32 is Copy, x still valid
    println!("{}", x);              // OK!
}
```

### Error Handling
Use `Result` and `Option` instead of exceptions:
```rust
use anyhow::{Context, Result};

fn process_file(path: &str) -> Result<()> {
    let contents = std::fs::read_to_string(path)
        .context("Failed to read file")?;

    // Process contents...
    Ok(())
}
```

### Zero-Cost Abstractions
High-level code compiles to efficient machine code:
```rust
// Iterator chain - as fast as hand-written loop
let sum: i32 = numbers
    .iter()
    .filter(|&&x| x % 2 == 0)
    .map(|&x| x * 2)
    .sum();
```

## Best Practices

1. **Use Clippy**: Run before every commit
2. **Format with rustfmt**: Consistent style
3. **Handle errors explicitly**: No `unwrap()` in libraries
4. **Write tests**: Unit and integration tests
5. **Document public APIs**: Use `///` doc comments
6. **Follow API Guidelines**: See Rust API Guidelines
7. **Use iterators**: Functional and efficient
8. **Leverage type system**: Make invalid states unrepresentable

## Modern Rust (2024/2025)

### Edition 2021
- Disjoint capture in closures
- IntoIterator for arrays
- Panic macro consistency

### Upcoming Features
- Async traits (stabilized)
- Generic associated types (GATs)
- Let-else statements
- Edition 2024 (coming soon)

## Resources

### Official Documentation
- [The Rust Programming Language](https://doc.rust-lang.org/book/)
- [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/)
- [The Cargo Book](https://doc.rust-lang.org/cargo/)
- [Clippy Lints](https://rust-lang.github.io/rust-clippy/)

### Error Handling
- [thiserror](https://docs.rs/thiserror/)
- [anyhow](https://docs.rs/anyhow/)
- [Error Handling Guide 2025](https://markaicode.com/rust-error-handling-2025-guide/)

### Testing & Benchmarking
- [Criterion.rs](https://github.com/bheisler/criterion.rs)
- [Rust Benchmarking Guide](https://www.rustfinity.com/blog/rust-benchmarking-with-criterion)

### Community Resources
- [Rust Users Forum](https://users.rust-lang.org/)
- [This Week in Rust](https://this-week-in-rust.org/)
- [Awesome Rust](https://github.com/rust-unofficial/awesome-rust)
