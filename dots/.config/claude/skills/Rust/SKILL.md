---
name: Rust
description: Rust development best practices and patterns. USE WHEN writing Rust code, designing Rust projects, working with Cargo, testing, or Rust package development.
---

# Rust Development Best Practices

## Purpose
Guide Rust development following official standards, community best practices, and idiomatic patterns from The Rust Book, Rust API Guidelines, and the Rust team's recommendations.

### Context Detection

**This skill activates when:**
- Claude is asked to work on Rust code
- Current directory path or git repository contains `Cargo.toml`
- User is working with `.rs` files
- Commands like `cargo`, `rustc`, or `clippy` are mentioned

## Workflow Routing

**When executing a workflow, output this notification directly:**

```
Running the **WorkflowName** workflow from the **Rust** skill...
```

| Workflow | Trigger | File |
|----------|---------|------|
| **Build** | "build rust project", "cargo build", "compile" | `workflows/Build.md` |
| **Test** | "run tests", "cargo test", "test coverage" | `workflows/Test.md` |
| **Bench** | "benchmark", "cargo bench", "performance test" | `workflows/Bench.md` |
| **Lint** | "lint", "clippy", "cargo fmt", "format" | `workflows/Lint.md` |
| **Deps** | "dependencies", "cargo add", "update deps" | `workflows/Deps.md` |
| **Error** | "error handling", "Result", "thiserror", "anyhow" | `workflows/Error.md` |
| **Workspace** | "workspace", "multi-crate", "monorepo" | `workflows/Workspace.md` |
| **Publish** | "publish crate", "crates.io", "cargo publish" | `workflows/Publish.md` |

## Core Principles

1. **Safety first**: Leverage Rust's ownership and type system for memory safety
2. **Zero-cost abstractions**: Write high-level code without runtime overhead
3. **Fearless concurrency**: Use Rust's guarantees to write concurrent code safely
4. **Explicit error handling**: Use `Result` and `Option` types, never panic in libraries
5. **Idiomatic patterns**: Follow Rust API Guidelines and community conventions

## Standard Project Structure

### Binary Application
```
myapp/
├── Cargo.toml              # Project manifest
├── Cargo.lock              # Dependency lock file (commit for apps)
├── src/
│   ├── main.rs            # Entry point
│   ├── lib.rs             # Optional library code
│   └── bin/               # Additional binaries
│       └── helper.rs
├── tests/                  # Integration tests
│   └── integration_test.rs
├── benches/                # Benchmarks
│   └── my_bench.rs
├── examples/               # Example code
│   └── basic.rs
└── README.md
```

### Library Crate
```
mylib/
├── Cargo.toml
├── Cargo.lock              # Don't commit for libraries
├── src/
│   ├── lib.rs             # Library root
│   └── submodule/
│       └── mod.rs
├── tests/
│   └── integration_test.rs
├── benches/
│   └── benchmark.rs
└── examples/
    └── usage.rs
```

### Workspace (Multiple Crates)
```
myworkspace/
├── Cargo.toml              # Workspace manifest
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
└── README.md
```

## Writing Idiomatic Rust

### Naming Conventions

```rust
// Types: UpperCamelCase
struct MyStruct;
enum MyEnum { VariantA, VariantB }
trait MyTrait {}

// Functions, variables, modules: snake_case
fn my_function() {}
let my_variable = 42;
mod my_module {}

// Constants, statics: SCREAMING_SNAKE_CASE
const MAX_SIZE: usize = 100;
static GLOBAL_COUNT: AtomicUsize = AtomicUsize::new(0);

// Lifetimes: short lowercase
fn foo<'a, 'b>(x: &'a str, y: &'b str) -> &'a str { x }
```

### Error Handling

```rust
use std::fs::File;
use std::io::{self, Read};

// Good: Use Result for recoverable errors
fn read_file(path: &str) -> Result<String, io::Error> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

// Good: Use Option for values that might not exist
fn find_user(id: u64) -> Option<User> {
    DATABASE.get(&id).cloned()
}

// Bad: Don't use unwrap() in library code
fn bad_read_file(path: &str) -> String {
    let mut file = File::open(path).unwrap(); // DON'T DO THIS
    // ...
}

// Use thiserror for library errors
use thiserror::Error;

#[derive(Error, Debug)]
pub enum MyError {
    #[error("IO error: {0}")]
    Io(#[from] io::Error),

    #[error("Parse error: {0}")]
    Parse(String),

    #[error("Not found: {0}")]
    NotFound(String),
}

// Use anyhow for application errors
use anyhow::{Context, Result};

fn process_file(path: &str) -> Result<()> {
    let contents = std::fs::read_to_string(path)
        .context("Failed to read configuration file")?;

    let config: Config = serde_json::from_str(&contents)
        .context("Failed to parse JSON config")?;

    Ok(())
}
```

### Ownership and Borrowing

```rust
// Good: Use references when you don't need ownership
fn calculate_length(s: &String) -> usize {
    s.len()
}

// Good: Use mutable references for modification
fn append_world(s: &mut String) {
    s.push_str(", world!");
}

// Good: Take ownership when you need it
fn consume_string(s: String) -> usize {
    s.len()
} // s is dropped here

// Good: Use slices for flexibility
fn first_word(s: &str) -> &str {
    let bytes = s.as_bytes();
    for (i, &byte) in bytes.iter().enumerate() {
        if byte == b' ' {
            return &s[..i];
        }
    }
    &s[..]
}
```

### Iterator Patterns

```rust
// Good: Use iterators for functional transformations
let numbers: Vec<i32> = vec![1, 2, 3, 4, 5];

let doubled: Vec<i32> = numbers
    .iter()
    .map(|x| x * 2)
    .collect();

let sum: i32 = numbers
    .iter()
    .filter(|&&x| x % 2 == 0)
    .sum();

// Good: Chain iterator methods
let result: Vec<String> = input
    .lines()
    .map(str::trim)
    .filter(|line| !line.is_empty())
    .map(|line| line.to_uppercase())
    .collect();

// Good: Use iterator adapters
let pairs: Vec<(usize, &i32)> = numbers
    .iter()
    .enumerate()
    .filter(|(i, _)| i % 2 == 0)
    .collect();
```

## Cargo Essentials

### Cargo.toml Configuration

```toml
[package]
name = "myproject"
version = "0.1.0"
edition = "2021"           # Use latest edition (2024 coming soon)
rust-version = "1.70"      # Minimum supported Rust version (MSRV)
authors = ["Your Name <you@example.com>"]
description = "A short description"
documentation = "https://docs.rs/myproject"
repository = "https://github.com/user/myproject"
license = "MIT OR Apache-2.0"
keywords = ["cli", "tool"]
categories = ["command-line-utilities"]

[dependencies]
serde = { version = "1.0", features = ["derive"] }
tokio = { version = "1.0", features = ["full"] }
anyhow = "1.0"

[dev-dependencies]
criterion = "0.7"
proptest = "1.0"

[build-dependencies]
cc = "1.0"

# Profile optimizations
[profile.release]
lto = true              # Link-time optimization
codegen-units = 1       # Better optimization
strip = true            # Remove debug symbols
opt-level = 3           # Maximum optimization

[profile.dev]
opt-level = 0           # Fast compilation
debug = true

# Workspace dependencies (in workspace root)
[workspace.dependencies]
serde = "1.0"
tokio = "1.0"
```

### Common Commands

```bash
# Create new project
cargo new myproject          # Binary
cargo new --lib mylib        # Library

# Build
cargo build                  # Debug build
cargo build --release        # Release build
cargo build --all-features   # With all features

# Run
cargo run                    # Run binary
cargo run --example basic    # Run example

# Test
cargo test                   # Run all tests
cargo test --doc            # Run doc tests
cargo test integration      # Run tests matching pattern

# Check without building
cargo check                  # Fast compile check
cargo clippy                # Lint checks

# Format
cargo fmt                    # Format code

# Documentation
cargo doc --open            # Build and open docs

# Dependencies
cargo add serde             # Add dependency
cargo update                # Update dependencies
cargo tree                  # Show dependency tree
```

## Testing

### Unit Tests

```rust
// In same file as code being tested
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_addition() {
        assert_eq!(add(2, 2), 4);
    }

    #[test]
    fn test_with_result() -> Result<(), String> {
        if add(2, 2) == 4 {
            Ok(())
        } else {
            Err(String::from("Addition failed"))
        }
    }

    #[test]
    #[should_panic(expected = "divided by zero")]
    fn test_panic() {
        divide(10, 0);
    }

    #[test]
    #[ignore]  // Skip unless --ignored flag
    fn expensive_test() {
        // ...
    }
}
```

### Integration Tests

```rust
// tests/integration_test.rs
use mylib;

#[test]
fn it_works() {
    assert_eq!(mylib::add(2, 2), 4);
}

#[test]
fn test_full_workflow() {
    let result = mylib::process_data("input.txt");
    assert!(result.is_ok());
}
```

### Property-Based Testing

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_reversing_twice_gives_original(s in ".*") {
        let reversed = reverse(&s);
        let double_reversed = reverse(&reversed);
        prop_assert_eq!(s, double_reversed);
    }

    #[test]
    fn test_addition_commutative(a in 0..1000i32, b in 0..1000i32) {
        prop_assert_eq!(a + b, b + a);
    }
}
```

## Async Rust with Tokio

```rust
use tokio;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Spawn concurrent tasks
    let handle = tokio::spawn(async {
        // Async work
        fetch_data().await
    });

    let result = handle.await?;
    Ok(())
}

async fn fetch_data() -> Result<String, reqwest::Error> {
    let response = reqwest::get("https://api.example.com/data")
        .await?
        .text()
        .await?;
    Ok(response)
}

// Concurrent execution
use tokio::join;

async fn do_stuff_concurrently() {
    let (result1, result2, result3) = join!(
        fetch_thing_1(),
        fetch_thing_2(),
        fetch_thing_3()
    );
}
```

## Best Practices Checklist

- [ ] Use `cargo fmt` for consistent formatting
- [ ] Run `cargo clippy` and fix all warnings
- [ ] Add documentation comments (`///`) for public APIs
- [ ] Write tests for new functionality
- [ ] Use `Result` for error handling, not `panic!` in libraries
- [ ] Specify MSRV in `Cargo.toml`
- [ ] Use workspace dependencies for consistency
- [ ] Enable useful Clippy lints in `Cargo.toml`
- [ ] Profile release builds for performance
- [ ] Use semantic versioning for releases

## Resources

### Official Documentation
- [The Rust Programming Language (Book)](https://doc.rust-lang.org/book/)
- [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/)
- [The Cargo Book](https://doc.rust-lang.org/cargo/)
- [Clippy Lints](https://rust-lang.github.io/rust-clippy/)

### Error Handling
- [thiserror documentation](https://docs.rs/thiserror/)
- [anyhow documentation](https://docs.rs/anyhow/)
- [Error Handling Guide 2025](https://markaicode.com/rust-error-handling-2025-guide/)

### Testing & Benchmarking
- [Criterion.rs](https://github.com/bheisler/criterion.rs)
- [Rust Benchmarking Guide](https://www.rustfinity.com/blog/rust-benchmarking-with-criterion)

### Best Practices
- [Rust Project Structure Guide](https://www.djamware.com/post/68b2c7c451ce620c6f5efc56/rust-project-structure-and-best-practices-for-clean-scalable-code)
- [Large Rust Workspaces](https://matklad.github.io/2021/08/22/large-rust-workspaces.html)

## Examples

**Example 1: Creating a Rust CLI application**
```
User: "Build a CLI tool to process logs"
→ Initializes project with cargo new
→ Adds clap for argument parsing
→ Implements error handling with anyhow
→ Writes unit tests and integration tests
→ Builds optimized binary with cargo build --release
→ Result: Fast, safe Rust CLI tool
```

**Example 2: Fixing ownership and borrowing issues**
```
User: "Help me fix these borrow checker errors"
→ Analyzes ownership flow in code
→ Identifies unnecessary clones or moves
→ Suggests lifetime annotations where needed
→ Refactors to use references appropriately
→ Explains Rust's ownership rules
→ Result: Clean code that satisfies the borrow checker
```

**Example 3: Optimizing Rust performance**
```
User: "This Rust code is slower than expected"
→ Uses cargo flamegraph to profile
→ Identifies allocations and hot paths
→ Optimizes with iterators and zero-copy operations
→ Adds benchmark tests with criterion
→ Measures improvement
→ Result: Performant Rust code with benchmarks
```
