# Lint Workflow

Lint and format Rust code with Clippy and rustfmt.

## When to Use

- "lint rust code"
- "cargo clippy"
- "cargo fmt"
- "format rust"

## Quick Commands

### Clippy (Linter)

```bash
# Run clippy
cargo clippy

# Fix automatically
cargo clippy --fix

# Treat warnings as errors
cargo clippy -- -D warnings

# All targets and features
cargo clippy --all-targets --all-features -- -D warnings

# Specific lint levels
cargo clippy -- -W clippy::pedantic
cargo clippy -- -W clippy::nursery
```

### rustfmt (Formatter)

```bash
# Format code
cargo fmt

# Check if formatted
cargo fmt -- --check

# Format specific file
rustfmt src/main.rs
```

## Configuration

### clippy.toml

```toml
# .clippy.toml or clippy.toml
cognitive-complexity-threshold = 30
type-complexity-threshold = 250
too-many-arguments-threshold = 8
```

### rustfmt.toml

```toml
# rustfmt.toml
max_width = 100
hard_tabs = false
tab_spaces = 4
newline_style = "Unix"
use_small_heuristics = "Default"
reorder_imports = true
reorder_modules = true
remove_nested_parens = true
edition = "2021"
```

### Cargo.toml Lints

```toml
[lints.rust]
unsafe_code = "forbid"
missing_docs = "warn"

[lints.clippy]
enum_glob_use = "deny"
pedantic = "warn"
nursery = "warn"
unwrap_used = "deny"
```

## Common Clippy Lints

### Performance

```rust
// clippy::unnecessary_clone
let s = string.clone(); // Unnecessary if string not used after

// clippy::needless_collect
let v: Vec<_> = iter.collect();
let len = v.len(); // Better: iter.count()

// clippy::single_char_pattern
s.split("x") // Better: s.split('x')
```

### Correctness

```rust
// clippy::eq_op
if x == x { } // Always true

// clippy::clone_on_copy
let x = 5.clone(); // i32 is Copy, not Clone

// clippy::unnecessary_mut_passed
fn takes_ref(x: &i32) {}
let mut x = 5;
takes_ref(&mut x); // &mut not needed
```

### Style

```rust
// clippy::needless_return
fn foo() -> i32 {
    return 42; // return keyword unnecessary
}

// clippy::collapsible_if
if x {
    if y { }
}
// Better: if x && y { }
```

## Allowing Lints

### File Level

```rust
#![allow(clippy::module_name_repetitions)]
```

### Function Level

```rust
#[allow(clippy::too_many_arguments)]
fn complex_function(a: i32, b: i32, c: i32, d: i32, e: i32) {}
```

### Inline

```rust
#[allow(clippy::cast_possible_truncation)]
let x = long_value as u32;
```

## CI Integration

```yaml
# .github/workflows/ci.yml
name: CI

on: [push, pull_request]

jobs:
  lint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
        with:
          components: clippy, rustfmt
      - run: cargo fmt -- --check
      - run: cargo clippy --all-targets --all-features -- -D warnings
```

## Best Practices

1. **Run before committing** - Fix issues early
2. **Use `--fix`** - Auto-fix when possible
3. **Configure in Cargo.toml** - Project-specific lints
4. **Deny in CI** - `-- -D warnings`
5. **Allow sparingly** - Document why when needed
6. **Update regularly** - New lints in new Rust versions
