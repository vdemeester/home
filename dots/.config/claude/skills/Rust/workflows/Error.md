# Error Handling Workflow

Handle errors idiomatically in Rust using Result, Option, thiserror, and anyhow.

## When to Use

- "error handling"
- "Result type"
- "thiserror"
- "anyhow"

## Core Types

### Result<T, E>

```rust
fn read_file(path: &str) -> Result<String, std::io::Error> {
    std::fs::read_to_string(path)
}

// Usage
match read_file("config.txt") {
    Ok(contents) => println!("{}", contents),
    Err(e) => eprintln!("Error: {}", e),
}

// With ? operator
fn process() -> Result<(), std::io::Error> {
    let contents = read_file("config.txt")?;
    println!("{}", contents);
    Ok(())
}
```

### Option<T>

```rust
fn find_user(id: u64) -> Option<User> {
    DATABASE.get(&id).cloned()
}

// Usage
match find_user(42) {
    Some(user) => println!("Found: {}", user.name),
    None => println!("Not found"),
}

// With ? operator (in function returning Option)
fn get_user_email(id: u64) -> Option<String> {
    let user = find_user(id)?;
    Some(user.email)
}
```

## Library Errors: thiserror

Use `thiserror` to define custom error types for libraries.

```toml
[dependencies]
thiserror = "2.0"
```

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum MyError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Parse error on line {line}: {msg}")]
    Parse { line: usize, msg: String },

    #[error("User {0} not found")]
    NotFound(String),

    #[error("Invalid configuration")]
    InvalidConfig,
}

// Usage
fn read_config(path: &str) -> Result<Config, MyError> {
    let contents = std::fs::read_to_string(path)?; // Auto-converts io::Error

    parse_config(&contents)
        .map_err(|e| MyError::Parse {
            line: e.line,
            msg: e.to_string(),
        })
}
```

### Automatic Conversions

```rust
#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]  // Use inner error's Display
    Io(#[from] std::io::Error),

    #[error(transparent)]
    Parse(#[from] serde_json::Error),
}

// Both io::Error and serde_json::Error auto-convert
fn load_json(path: &str) -> Result<Data, Error> {
    let contents = std::fs::read_to_string(path)?; // io::Error -> Error
    let data = serde_json::from_str(&contents)?;   // serde_json::Error -> Error
    Ok(data)
}
```

## Application Errors: anyhow

Use `anyhow` for applications where you want flexible error handling.

```toml
[dependencies]
anyhow = "2.0"
```

```rust
use anyhow::{Context, Result};

fn process_file(path: &str) -> Result<()> {
    let contents = std::fs::read_to_string(path)
        .context("Failed to read configuration file")?;

    let config: Config = serde_json::from_str(&contents)
        .context("Failed to parse JSON configuration")?;

    apply_config(config)
        .context("Failed to apply configuration")?;

    Ok(())
}

// Custom errors with context
fn validate_user(user: &User) -> Result<()> {
    anyhow::ensure!(
        user.age >= 18,
        "User must be 18 or older, got {}",
        user.age
    );

    if user.email.is_empty() {
        anyhow::bail!("Email cannot be empty");
    }

    Ok(())
}
```

## When to Use What

### thiserror - For Libraries

```rust
// Define typed errors for library boundaries
#[derive(Error, Debug)]
pub enum DatabaseError {
    #[error("Connection failed: {0}")]
    Connection(String),

    #[error("Query error: {0}")]
    Query(#[from] sqlx::Error),
}

pub fn connect(url: &str) -> Result<Connection, DatabaseError> {
    // Library code with typed errors
}
```

### anyhow - For Applications

```rust
// Application layer with flexible error handling
use anyhow::Result;

fn main() -> Result<()> {
    let config = load_config("config.toml")
        .context("Failed to load configuration")?;

    let db = connect(&config.database_url)
        .context("Database connection failed")?;

    run_server(db, config)?;
    Ok(())
}
```

## Error Patterns

### Wrapping Errors

```rust
#[derive(Error, Debug)]
pub enum AppError {
    #[error("Database error")]
    Database(#[from] DatabaseError),

    #[error("Network error")]
    Network(#[from] NetworkError),
}
```

### Adding Context

```rust
// With anyhow
file.read()
    .context("Reading user data")
    .with_context(|| format!("File: {}", path))?;

// With Result
file.read()
    .map_err(|e| format!("Failed to read {}: {}", path, e))?;
```

### Recovering from Errors

```rust
// Using or_else
let value = risky_operation().unwrap_or_else(|_| default_value());

// Using or
let value = optional_value.or(Some(default));

// Using ok_or
let result: Result<T, E> = option.ok_or(error)?;
```

## Best Practices

1. **Libraries: use thiserror** - Typed errors at API boundaries
2. **Applications: use anyhow** - Flexible error aggregation
3. **Never panic in libraries** - Return `Result` instead
4. **Add context** - Use `.context()` for meaningful errors
5. **Use ? operator** - Propagate errors ergonomically
6. **Document errors** - List possible errors in docs
7. **Don't use `unwrap()`** - Except in tests or when impossible to fail

### Don't Do This

```rust
// Bad: panic in library
pub fn get_user(id: u64) -> User {
    DATABASE.get(&id).unwrap() // DON'T!
}

// Bad: losing error information
fn bad() -> Result<(), String> {
    file.read().map_err(|_| "failed".to_string())?; // Lost error details
    Ok(())
}
```

### Do This

```rust
// Good: return Result
pub fn get_user(id: u64) -> Result<User, Error> {
    DATABASE.get(&id)
        .ok_or(Error::NotFound(id))
}

// Good: preserve error information
fn good() -> Result<()> {
    file.read()
        .context("Failed to read user data")?;
    Ok(())
}
```
