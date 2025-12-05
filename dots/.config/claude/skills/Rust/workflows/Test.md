# Test Workflow

Run Rust tests with Cargo.

## When to Use

- "run rust tests"
- "cargo test"
- "test coverage"

## Quick Commands

```bash
# Run all tests
cargo test

# Run tests with output
cargo test -- --nocapture

# Run specific test
cargo test test_name

# Run tests in specific module
cargo test module_name::

# Run doc tests only
cargo test --doc

# Run tests in release mode
cargo test --release

# Run ignored tests
cargo test -- --ignored

# Run tests in parallel (default)
cargo test

# Run tests single-threaded
cargo test -- --test-threads=1
```

## Test Types

### Unit Tests

```rust
// src/lib.rs
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(add(2, 2), 4);
    }

    #[test]
    fn test_add_negative() {
        assert_eq!(add(-1, 1), 0);
    }

    #[test]
    #[should_panic(expected = "overflow")]
    fn test_overflow() {
        let _result = add(i32::MAX, 1);
    }

    #[test]
    #[ignore]
    fn expensive_test() {
        // Run only with --ignored
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
fn test_full_process() {
    let result = mylib::process("input");
    assert!(result.is_ok());
}
```

### Doc Tests

```rust
/// Adds two numbers.
///
/// # Examples
///
/// ```
/// use mylib::add;
/// assert_eq!(add(2, 2), 4);
/// ```
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}
```

## Assertions

```rust
#[test]
fn test_assertions() {
    // Equality
    assert_eq!(2 + 2, 4);
    assert_ne!(2 + 2, 5);

    // Boolean
    assert!(true);
    assert!(!false);

    // Custom message
    assert_eq!(2 + 2, 4, "Math is broken: {} != {}", 2 + 2, 4);

    // Debug assertion (only in debug builds)
    debug_assert_eq!(expensive_computation(), expected);
}
```

## Test Coverage

### Install tarpaulin

```bash
cargo install cargo-tarpaulin
```

### Run Coverage

```bash
# Generate coverage report
cargo tarpaulin --out Html --output-dir coverage

# With specific format
cargo tarpaulin --out Lcov
cargo tarpaulin --out Xml
```

## Property-Based Testing

```toml
[dev-dependencies]
proptest = "1.0"
```

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_reverse_twice(s in ".*") {
        let rev = reverse(&s);
        let rev_rev = reverse(&rev);
        prop_assert_eq!(s, rev_rev);
    }
}
```

## Mocking

```toml
[dev-dependencies]
mockall = "0.12"
```

```rust
use mockall::{automock, predicate::*};

#[automock]
trait Database {
    fn get_user(&self, id: u64) -> Option<User>;
}

#[test]
fn test_with_mock() {
    let mut mock = MockDatabase::new();
    mock.expect_get_user()
        .with(eq(1))
        .returning(|_| Some(User::default()));

    assert!(mock.get_user(1).is_some());
}
```

## Best Practices

1. **Test public APIs** - Focus on behavior, not implementation
2. **Use descriptive names** - `test_user_login_with_invalid_credentials`
3. **One assertion per test** - When practical
4. **Use `Result<()>` return type** - For tests that can fail
5. **Write doc tests** - Examples and tests in one
6. **Property-based tests** - For invariants
7. **Run tests in CI** - Catch regressions early
