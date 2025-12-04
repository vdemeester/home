# Debug Workflow

Debug Go applications using delve debugger and other debugging techniques.

## When to Use

- "debug go code"
- "use delve"
- "troubleshoot go program"
- "debug race condition"
- "inspect goroutines"

## Quick Commands

### Using delve
```bash
# Install delve
go install github.com/go-delve/delve/cmd/dlv@latest

# Debug current package
dlv debug

# Debug with arguments
dlv debug -- arg1 arg2

# Debug tests
dlv test

# Attach to running process
dlv attach <pid>

# Debug specific test
dlv test -- -test.run TestMyFunction
```

## Delve Commands

### Breakpoints
```
# Set breakpoint at function
break main.main
b main.main

# Set breakpoint at file:line
break main.go:42
b main.go:42

# Set conditional breakpoint
break main.go:42 if x > 5

# List breakpoints
breakpoints
bp

# Clear breakpoint
clear 1

# Clear all breakpoints
clearall
```

### Execution Control
```
# Continue execution
continue
c

# Step over (next line)
next
n

# Step into (follow function call)
step
s

# Step out (return from function)
stepout

# Restart
restart
r
```

### Inspection
```
# Print variable
print varName
p varName

# Print with format
p fmt.Sprintf("%#v", varName)

# List local variables
locals

# List function arguments
args

# Show current goroutines
goroutines

# Show stack trace
stack
bt

# List source code
list
l

# List around specific line
list main.go:42
```

### Goroutine Debugging
```
# List all goroutines
goroutines

# Switch to goroutine
goroutine <id>

# Show goroutine stack
goroutine <id> bt
```

## Print Debugging

### Strategic Logging
```go
import "log"

func problematicFunction() {
    log.Printf("DEBUG: Entering function, value=%v", someValue)

    // Complex logic
    result := compute()
    log.Printf("DEBUG: After compute, result=%v", result)

    return result
}
```

### Using fmt.Printf
```go
// Quick debug prints
fmt.Printf("DEBUG: x=%#v\n", x)          // Detailed format
fmt.Printf("DEBUG: type=%T value=%v\n", x, x)  // Type and value
```

### Conditional Debug Output
```go
const debug = false

func debugPrintf(format string, args ...interface{}) {
    if debug {
        log.Printf("DEBUG: "+format, args...)
    }
}

func myFunction() {
    debugPrintf("Processing %d items", len(items))
}
```

## Race Condition Debugging

### Detect Races
```bash
# Build with race detector
go build -race

# Run with race detector
go run -race main.go

# Test with race detector
go test -race ./...
```

### Race Detector Output
```
==================
WARNING: DATA RACE
Read at 0x00c000100000 by goroutine 7:
  main.read()
      /path/to/file.go:10 +0x44

Previous write at 0x00c000100000 by goroutine 6:
  main.write()
      /path/to/file.go:15 +0x56
==================
```

### Fix Races with Mutex
```go
// Before (race condition)
var counter int

func increment() {
    counter++  // Race!
}

// After (safe)
var (
    counter int
    mu      sync.Mutex
)

func increment() {
    mu.Lock()
    defer mu.Unlock()
    counter++
}
```

## Debugging Patterns

### Panic Recovery
```go
defer func() {
    if r := recover(); r != nil {
        fmt.Printf("Recovered from panic: %v\n", r)
        debug.PrintStack()
    }
}()
```

### Detailed Stack Traces
```go
import "runtime/debug"

func logPanic() {
    if r := recover(); r != nil {
        log.Printf("Panic: %v\n%s", r, debug.Stack())
    }
}
```

### Debugging HTTP Handlers
```go
func debugMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        log.Printf("Request: %s %s", r.Method, r.URL.Path)

        // Capture response
        rec := httptest.NewRecorder()
        next.ServeHTTP(rec, r)

        log.Printf("Response: %d", rec.Code)

        // Copy to actual response
        for k, v := range rec.Header() {
            w.Header()[k] = v
        }
        w.WriteHeader(rec.Code)
        w.Write(rec.Body.Bytes())
    })
}
```

## Environment Variables for Debugging

```bash
# Enable detailed GC logging
GODEBUG=gctrace=1 go run main.go

# Show goroutine scheduling
GODEBUG=schedtrace=1000 go run main.go

# Show memory allocator
GODEBUG=allocfreetrace=1 go run main.go

# Multiple debug settings
GODEBUG=gctrace=1,schedtrace=1000 go run main.go
```

## VSCode Integration

### launch.json Example
```json
{
    "version": "0.2.0",
    "configurations": [
        {
            "name": "Debug Package",
            "type": "go",
            "request": "launch",
            "mode": "debug",
            "program": "${workspaceFolder}",
            "args": ["arg1", "arg2"]
        },
        {
            "name": "Debug Test",
            "type": "go",
            "request": "launch",
            "mode": "test",
            "program": "${workspaceFolder}",
            "args": ["-test.run", "TestMyFunction"]
        }
    ]
}
```

## Common Issues

### Deadlock Detection
```go
// Go runtime detects deadlocks
fatal error: all goroutines are asleep - deadlock!

goroutine 1 [chan receive]:
main.main()
    /path/to/file.go:10 +0x50
```

### Memory Leaks
```bash
# Profile memory
go test -memprofile=mem.prof

# Analyze
go tool pprof mem.prof

# Look for:
# - Growing heap
# - Goroutine leaks
# - Unclosed resources
```

## Best Practices

1. **Use delve for complex issues**: More powerful than print debugging
2. **Enable race detector in tests**: Catch concurrency bugs early
3. **Log strategically**: Don't over-log, focus on problem areas
4. **Use proper log levels**: Debug, Info, Warn, Error
5. **Reproduce in tests**: Write failing test before debugging
6. **Check goroutine leaks**: Use pprof to inspect goroutines
7. **Validate assumptions**: Use assertions and invariants

## Resources

- [Delve Debugger](https://github.com/go-delve/delve)
- [Debugging with Go](https://go.dev/doc/diagnostics)
- [Race Detector](https://go.dev/doc/articles/race_detector)
