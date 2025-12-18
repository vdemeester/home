---
name: Golang
description: Go development best practices and patterns. USE WHEN writing Go code, designing Go projects, working with Go tools, testing, or Go package development.
---

# Go Development Best Practices

## Purpose
Guide Go development following official standards, community best practices, and idiomatic patterns from Effective Go and the Go team's code review guidelines.

### Context Detection

**This skill actives when:**
- Claude is asked to work on Go code
- Current directory path or current git repository contains go.mod

## Workflow Routing

**When executing a workflow, output this notification directly:**

```
Running the **WorkflowName** workflow from the **Golang** skill...
```

| Workflow | Trigger | File |
|----------|---------|------|
| **Build** | "build go project", "compile", "cross compile" | `workflows/Build.md` |
| **Test** | "run tests", "test coverage", "race detector" | `workflows/Test.md` |
| **Benchmark** | "benchmark", "measure performance", "profile" | `workflows/Benchmark.md` |
| **Lint** | "lint", "format", "gofmt", "golangci-lint" | `workflows/Lint.md` |
| **Deps** | "dependencies", "go mod", "update deps", "vendor" | `workflows/Deps.md` |
| **Debug** | "debug", "delve", "troubleshoot" | `workflows/Debug.md` |
| **Profile** | "profile performance", "pprof", "optimize" | `workflows/Profile.md` |
| **Generate** | "generate code", "mockgen", "stringer", "protobuf" | `workflows/Generate.md` |

## Core Principles

1. **Clarity over cleverness**: Write simple, readable code that's easy to maintain
2. **Gofmt is law**: Always format code with `gofmt` - no exceptions
3. **Handle errors explicitly**: Errors are values that must be deliberately processed
4. **Share by communicating**: Use channels and goroutines for concurrency
5. **Prefer composition**: Embed types and use interfaces over inheritance patterns

## Standard Project Structure

### Recommended Layout
```
myproject/
├── cmd/
│   └── myapp/
│       └── main.go          # Application entry points
├── internal/                # Private application code
│   ├── config/
│   ├── handlers/
│   └── models/
├── pkg/                     # Public library code (optional)
│   └── utils/
├── api/                     # API definitions (OpenAPI, protobuf)
├── configs/                 # Configuration files
├── docs/                    # Documentation
├── scripts/                 # Build and analysis scripts
├── test/                    # Additional test data
├── go.mod                   # Module definition
├── go.sum                   # Dependency checksums
├── README.md
└── *_test.go               # Tests alongside code
```

### Key Directories

- **`cmd/`**: Main applications - each subdirectory is a separate binary
- **`internal/`**: Private code that cannot be imported by other projects
- **`pkg/`**: Public libraries that can be imported by external projects
- **`api/`**: API specifications and protocol definitions

**Start simple**: Don't create all directories upfront. Add structure as complexity grows.

## Writing Idiomatic Go

### Formatting and Style

**Always use gofmt**
```bash
# Format all files in current directory and subdirectories
gofmt -w .

# Use goimports to also manage imports
goimports -w .
```

**Follow standard formatting**:
- Use tabs for indentation (gofmt handles this)
- No strict line length, but wrap long lines for readability
- Group imports: standard library, third-party, local

```go
import (
    "fmt"
    "os"

    "github.com/pkg/errors"
    "github.com/spf13/cobra"

    "myproject/internal/config"
)
```

### Naming Conventions

**Packages**
- Lowercase, single-word names: `package config`, `package handler`
- No underscores or mixedCaps in package names
- Choose clear, concise names that describe the package's purpose

**Variables and Functions**
- Use `mixedCaps` or `MixedCaps`, never underscores
- Exported names start with uppercase: `UserController`, `ProcessData`
- Private names start with lowercase: `userCount`, `processRequest`
- Short names in small scopes: `i`, `err`, `ok`, `buf`
- Descriptive names in larger scopes: `userRepository`, `configManager`

**Getters and Setters**
```go
// Good: Getters don't use "Get" prefix
user.Name()      // not user.GetName()
user.SetName(n)  // Setter keeps "Set"

// Good: For interfaces with single method
type Reader interface {
    Read(p []byte) (n int, err error)
}
```

**Constants**
- Use `MixedCaps`: `MaxRetries`, `DefaultTimeout`
- Or group in const blocks with clear names

### Commentary and Documentation

**Package Comments**
```go
// Package regexp implements regular expression search.
//
// The syntax of the regular expressions accepted is:
//
//     regexp:
//         concatenation { '|' concatenation }
package regexp
```

**Function Comments**
```go
// Compile parses a regular expression and returns, if successful,
// a Regexp object that can be used to match against text.
func Compile(str string) (*Regexp, error) {
```

**Key principles**:
- Doc comments should be complete sentences
- Start with the name of the element being described
- Explain what, why, and any non-obvious behavior
- Document errors that can be returned

### Error Handling

**Check every error**
```go
// Good: Handle errors immediately
result, err := doSomething()
if err != nil {
    return fmt.Errorf("failed to do something: %w", err)
}

// Bad: Ignoring errors
result, _ := doSomething()  // Never do this
```

**Wrap errors with context**
```go
// Use %w to wrap errors for programmatic inspection
if err := os.Remove(file); err != nil {
    return fmt.Errorf("removing %s: %w", file, err)
}

// Use %v for simple annotation without inspection
if err := validate(data); err != nil {
    return fmt.Errorf("validation failed: %v", err)
}
```

**Check error types when needed**
```go
if errors.Is(err, os.ErrNotExist) {
    // Handle file not found
}

var pathErr *os.PathError
if errors.As(err, &pathErr) {
    // Access pathErr.Path, pathErr.Op
}
```

**Error handling patterns**
```go
// Good: Keep normal path at minimal indentation
value, err := someOperation()
if err != nil {
    return err
}
// Continue with value...

// Good: Early returns for error conditions
if user == nil {
    return errors.New("user cannot be nil")
}
if !user.IsActive {
    return errors.New("user is not active")
}
// Continue with valid user...
```

### Interfaces

**Design principles**
- Interfaces define behavior, not data
- Prefer small, focused interfaces (often single-method)
- Define interfaces where they're used, not where they're implemented
- Accept interfaces, return concrete types

```go
// Good: Small, focused interface
type Reader interface {
    Read(p []byte) (n int, err error)
}

// Good: Compose interfaces
type ReadWriter interface {
    Reader
    Writer
}

// Good: Define interface in consumer package
// Package needs something that can save data
type DataSaver interface {
    Save(data []byte) error
}
```

### Concurrency

**Goroutines**
```go
// Good: Make it clear when goroutines exit
func process(ctx context.Context) {
    done := make(chan bool)

    go func() {
        defer close(done)
        // Do work
        for {
            select {
            case <-ctx.Done():
                return
            default:
                doWork()
            }
        }
    }()

    <-done
}
```

**Channels**
```go
// Good: Specify channel direction in function signatures
func producer(out chan<- int) {
    out <- 42
    close(out)
}

func consumer(in <-chan int) {
    for val := range in {
        fmt.Println(val)
    }
}

// Good: Use buffered channels for resource limiting
requests := make(chan *Request, 100)
```

**Share by communicating**
```go
// Good: Use channels instead of shared memory
type result struct {
    value int
    err   error
}

resultCh := make(chan result)
go func() {
    val, err := compute()
    resultCh <- result{val, err}
}()

res := <-resultCh
if res.err != nil {
    // handle error
}
```

### Context Usage

**Always use context**
```go
// Good: Accept context as first parameter
func FetchUser(ctx context.Context, id string) (*User, error) {
    req, err := http.NewRequestWithContext(ctx, "GET", url, nil)
    if err != nil {
        return nil, err
    }
    // ...
}

// Good: Propagate context through call chains
func ProcessRequest(ctx context.Context, req *Request) error {
    user, err := FetchUser(ctx, req.UserID)
    if err != nil {
        return err
    }
    return SaveData(ctx, user)
}
```

### Function Design

**Keep functions small and focused**
```go
// Good: Single responsibility
func validateEmail(email string) error {
    if !strings.Contains(email, "@") {
        return errors.New("invalid email format")
    }
    return nil
}

func createUser(name, email string) (*User, error) {
    if err := validateEmail(email); err != nil {
        return nil, err
    }
    return &User{Name: name, Email: email}, nil
}
```

**Use options pattern for complex configuration**
```go
type ServerOption func(*Server)

func WithTimeout(d time.Duration) ServerOption {
    return func(s *Server) {
        s.timeout = d
    }
}

func NewServer(addr string, opts ...ServerOption) *Server {
    s := &Server{addr: addr, timeout: 30 * time.Second}
    for _, opt := range opts {
        opt(s)
    }
    return s
}

// Usage
srv := NewServer(":8080",
    WithTimeout(10*time.Second),
    WithMaxConns(100))
```

## Testing

### Table-Driven Tests

```go
func TestValidateEmail(t *testing.T) {
    tests := []struct {
        name    string
        email   string
        wantErr bool
    }{
        {
            name:    "valid email",
            email:   "user@example.com",
            wantErr: false,
        },
        {
            name:    "missing @",
            email:   "userexample.com",
            wantErr: true,
        },
        {
            name:    "empty email",
            email:   "",
            wantErr: true,
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            err := validateEmail(tt.email)
            if (err != nil) != tt.wantErr {
                t.Errorf("validateEmail(%q) error = %v, wantErr %v",
                    tt.email, err, tt.wantErr)
            }
        })
    }
}
```

### Test Organization

**Black-box vs White-box testing**
```go
// White-box: same package, can access internals
package mypackage

func TestInternalFunction(t *testing.T) { }

// Black-box: separate package, tests public API only
package mypackage_test

import "myproject/mypackage"

func TestPublicAPI(t *testing.T) { }
```

**Test helpers**
```go
// Mark test helpers with t.Helper()
func assertEqual(t *testing.T, got, want int) {
    t.Helper()
    if got != want {
        t.Errorf("got %d, want %d", got, want)
    }
}
```

**Use t.Error over t.Fatal**
```go
// Good: t.Error allows other tests to continue
func TestMultipleChecks(t *testing.T) {
    if got := compute(1); got != 2 {
        t.Errorf("compute(1) = %d, want 2", got)
    }
    if got := compute(2); got != 4 {
        t.Errorf("compute(2) = %d, want 4", got)
    }
}

// Use t.Fatal only when continuing makes no sense
func TestSetup(t *testing.T) {
    db, err := setupDatabase()
    if err != nil {
        t.Fatalf("setupDatabase() failed: %v", err)
    }
    // Can't continue without db
}
```

### Test Coverage

```bash
# Run tests with coverage
go test -cover ./...

# Generate coverage profile
go test -coverprofile=coverage.out ./...

# View coverage in browser
go tool cover -html=coverage.out

# Check coverage for specific packages
go test -cover ./internal/...
```

**Coverage guidelines**:
- Aim for meaningful coverage, not arbitrary percentages
- Focus on critical business logic and edge cases
- Don't test trivial code (getters, setters)
- Use coverage to find untested code paths, not as a goal

### Benchmarking

```go
func BenchmarkCompute(b *testing.B) {
    input := generateInput()

    b.ResetTimer() // Reset timer after setup
    for i := 0; i < b.N; i++ {
        compute(input)
    }
}

func BenchmarkComputeParallel(b *testing.B) {
    input := generateInput()

    b.RunParallel(func(pb *testing.PB) {
        for pb.Next() {
            compute(input)
        }
    })
}
```

```bash
# Run benchmarks
go test -bench=. ./...

# With memory allocation stats
go test -bench=. -benchmem ./...

# CPU profiling
go test -bench=. -cpuprofile=cpu.prof

# Analyze profile
go tool pprof cpu.prof
```

## Common Patterns

### Configuration Management

```go
type Config struct {
    Port     int           `json:"port" yaml:"port"`
    Timeout  time.Duration `json:"timeout" yaml:"timeout"`
    LogLevel string        `json:"log_level" yaml:"log_level"`
}

func LoadConfig(path string) (*Config, error) {
    data, err := os.ReadFile(path)
    if err != nil {
        return nil, fmt.Errorf("reading config file: %w", err)
    }

    var cfg Config
    if err := json.Unmarshal(data, &cfg); err != nil {
        return nil, fmt.Errorf("parsing config: %w", err)
    }

    return &cfg, nil
}
```

### Graceful Shutdown

```go
func main() {
    ctx, cancel := context.WithCancel(context.Background())
    defer cancel()

    // Listen for interrupt signals
    sigChan := make(chan os.Signal, 1)
    signal.Notify(sigChan, os.Interrupt, syscall.SIGTERM)

    go func() {
        <-sigChan
        log.Println("Shutdown signal received, cleaning up...")
        cancel()
    }()

    if err := run(ctx); err != nil {
        log.Fatalf("Application error: %v", err)
    }
}

func run(ctx context.Context) error {
    // Application logic with context
    <-ctx.Done()
    return cleanup()
}
```

### HTTP Client with Timeout

```go
// Good: Configure timeouts and connection pooling
client := &http.Client{
    Timeout: 10 * time.Second,
    Transport: &http.Transport{
        MaxIdleConns:        100,
        MaxIdleConnsPerHost: 10,
        IdleConnTimeout:     90 * time.Second,
        TLSHandshakeTimeout: 10 * time.Second,
    },
}

// Use context for per-request timeout
ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
defer cancel()

req, err := http.NewRequestWithContext(ctx, "GET", url, nil)
if err != nil {
    return err
}

resp, err := client.Do(req)
if err != nil {
    return err
}
defer resp.Body.Close()
```

### Resource Management with sync.Pool

```go
// Good: Reuse expensive objects
var bufferPool = sync.Pool{
    New: func() interface{} {
        return new(bytes.Buffer)
    },
}

func processData(data []byte) error {
    buf := bufferPool.Get().(*bytes.Buffer)
    defer bufferPool.Put(buf)
    buf.Reset()

    // Use buffer
    buf.Write(data)
    return process(buf)
}
```

## Common Pitfalls to Avoid

### ❌ Goroutine Leaks

```go
// Bad: Goroutine never exits
go func() {
    for {
        doWork()
    }
}()

// Good: Use context for cancellation
go func(ctx context.Context) {
    for {
        select {
        case <-ctx.Done():
            return
        default:
            doWork()
        }
    }
}(ctx)
```

### ❌ Race Conditions

```go
// Bad: Concurrent map access
var cache = make(map[string]string)

func get(key string) string {
    return cache[key]  // Race!
}

// Good: Use sync.Map for concurrent access
var cache sync.Map

func get(key string) string {
    val, _ := cache.Load(key)
    if val != nil {
        return val.(string)
    }
    return ""
}

// Or use mutex for complex operations
type Cache struct {
    mu   sync.RWMutex
    data map[string]string
}

func (c *Cache) Get(key string) string {
    c.mu.RLock()
    defer c.mu.RUnlock()
    return c.data[key]
}
```

### ❌ Defer in Loops

```go
// Bad: Defers accumulate until function exits
for _, file := range files {
    f, _ := os.Open(file)
    defer f.Close()  // Won't close until function exits!
}

// Good: Close immediately or use function wrapper
for _, file := range files {
    func() {
        f, err := os.Open(file)
        if err != nil {
            log.Printf("error opening %s: %v", file, err)
            return
        }
        defer f.Close()
        processFile(f)
    }()
}
```

### ❌ Pointer to Loop Variable

```go
// Bad: All goroutines will use the last value
for _, val := range values {
    go func() {
        process(val)  // Bug: val is the loop variable!
    }()
}

// Good: Pass value as parameter
for _, val := range values {
    go func(v string) {
        process(v)
    }(val)
}
```

### ❌ Nil Channel Operations

```go
// Bad: Sending/receiving on nil channel blocks forever
var ch chan int
ch <- 1  // Blocks forever!

// Good: Initialize channels
ch := make(chan int)
ch <- 1
```

## Tooling

### Essential Commands

```bash
# Format code
gofmt -w .
goimports -w .

# Vet code for common mistakes
go vet ./...

# Run tests
go test ./...
go test -v ./...          # Verbose
go test -cover ./...      # With coverage
go test -race ./...       # Race detector

# Dependency management
go mod tidy               # Clean up dependencies
go mod verify             # Verify dependencies
go get -u ./...           # Update dependencies

# Build
go build -o bin/myapp ./cmd/myapp
go install ./cmd/myapp

# Cross-compilation
GOOS=linux GOARCH=amd64 go build
GOOS=darwin GOARCH=arm64 go build
```

### Linting

```bash
# Install golangci-lint
go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest

# Run all linters
golangci-lint run

# Auto-fix issues
golangci-lint run --fix

# Run specific linters
golangci-lint run --enable=gosec,exhaustive
```

### Profiling

```bash
# CPU profiling
go test -cpuprofile cpu.prof -bench .
go tool pprof cpu.prof

# Memory profiling
go test -memprofile mem.prof -bench .
go tool pprof mem.prof

# Block profiling (find blocking goroutines)
go test -blockprofile block.prof -bench .
go tool pprof block.prof

# Analyze in browser
go tool pprof -http=:8080 cpu.prof
```

## Performance Best Practices

1. **Profile before optimizing**: Use `pprof` to find actual bottlenecks
2. **Use appropriate data structures**:
   - Slices for sequential access
   - Maps for key-value lookups
   - Channels for communication
3. **Preallocate when size is known**:
   ```go
   items := make([]Item, 0, expectedSize)
   ```
4. **Reuse objects with sync.Pool**: For frequently allocated objects
5. **Avoid unnecessary allocations**:
   - Use string builders instead of concatenation
   - Reuse buffers when possible
6. **Benchmark critical paths**: Write benchmarks for performance-critical code
7. **Use buffered channels appropriately**: Avoid blocking on channel operations

## Nix Integration

> **Note**: If you detect Nix usage in this project (presence of `flake.nix`, `default.nix`, or Nix package definitions), refer to `nix-integration.md` in this skill directory for detailed Nix-specific build instructions, package definitions, and integration patterns.

Quick check for Nix:
- Look for `flake.nix`, `default.nix`, or `shell.nix` in project root
- Check for `/pkgs` or `/tools` directories with Nix definitions
- User mentions Nix, NixOS, or buildGoModule

## Resources

### Official Documentation
- [Effective Go](https://go.dev/doc/effective_go) - Core Go programming guide
- [Go Code Review Comments](https://go.dev/wiki/CodeReviewComments) - Common code review issues
- [Go Standard Library](https://pkg.go.dev/std) - Documentation and examples
- [Google Go Style Guide](https://google.github.io/styleguide/go/) - Production best practices

### Tools
- [gofmt](https://pkg.go.dev/cmd/gofmt) - Code formatter
- [goimports](https://pkg.go.dev/golang.org/x/tools/cmd/goimports) - Import management
- [golangci-lint](https://golangci-lint.run/) - Comprehensive linter
- [gopls](https://github.com/golang/tools/tree/master/gopls) - Language server

### Learning Resources
- [Go by Example](https://gobyexample.com/) - Practical examples
- [Go Playground](https://go.dev/play/) - Try Go in browser
- [Uber Go Style Guide](https://github.com/uber-go/guide/blob/master/style.md) - Additional patterns

---

**Remember**: Write clear, simple, idiomatic Go code. When in doubt, consult Effective Go and the standard library for guidance. The Go community values simplicity, readability, and maintainability above cleverness.

**Sources**:
- [Effective Go](https://go.dev/doc/effective_go)
- [Go Code Review Comments](https://go.dev/wiki/CodeReviewComments)
- [Google Go Style Guide](https://google.github.io/styleguide/go/)
- [Go Best Practices 2025](https://www.bacancytechnology.com/blog/go-best-practices)
- [Go Coding Standards](https://leapcell.medium.com/go-coding-official-standards-and-best-practices-5e84f4dbc8ad)

## Examples

**Example 1: Creating a new Go CLI tool**
```
User: "Create a CLI tool to parse JSON logs"
→ Sets up project with go mod init
→ Creates main.go with cobra for CLI structure
→ Adds JSON parsing with encoding/json
→ Writes table-driven tests
→ Builds with proper flags and versioning
→ Result: Production-ready Go CLI tool
```

**Example 2: Writing idiomatic Go code**
```
User: "Add error handling to this function"
→ Reviews existing code for patterns
→ Adds proper error wrapping with fmt.Errorf
→ Uses early returns for error cases
→ Adds context.Context for cancellation
→ Writes tests for error paths
→ Result: Idiomatic Go with robust error handling
```

**Example 3: Optimizing Go performance**
```
User: "This function is slow, can you optimize it?"
→ Profiles with pprof to identify bottlenecks
→ Reduces allocations with sync.Pool
→ Uses appropriate data structures (map vs slice)
→ Adds benchmarks to measure improvement
→ Documents performance characteristics
→ Result: Measurably faster code with evidence
```
