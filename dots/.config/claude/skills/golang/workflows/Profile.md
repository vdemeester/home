# Profile Workflow

Advanced profiling and performance analysis for Go applications.

## When to Use

- "profile go application"
- "find performance bottlenecks"
- "analyze memory usage"
- "trace execution"
- "optimize performance"

## Types of Profiling

### CPU Profiling
```bash
# During tests
go test -cpuprofile=cpu.prof -bench=.

# In application
import _ "net/http/pprof"
go func() {
    log.Println(http.ListenAndServe("localhost:6060", nil))
}()

# Capture profile
curl http://localhost:6060/debug/pprof/profile?seconds=30 > cpu.prof

# Analyze
go tool pprof cpu.prof
```

### Memory Profiling
```bash
# Heap profile
go test -memprofile=mem.prof -bench=.

# From running app
curl http://localhost:6060/debug/pprof/heap > heap.prof

# Analyze
go tool pprof mem.prof

# Allocation profile
go tool pprof -alloc_space mem.prof
```

### Block Profiling
```bash
# Profile blocking operations
import "runtime"
runtime.SetBlockProfileRate(1)

# Capture
curl http://localhost:6060/debug/pprof/block > block.prof

# Analyze
go tool pprof block.prof
```

### Mutex Profiling
```bash
# Profile mutex contention
import "runtime"
runtime.SetMutexProfileFraction(1)

# Capture
curl http://localhost:6060/debug/pprof/mutex > mutex.prof

# Analyze
go tool pprof mutex.prof
```

## pprof Commands

### Interactive Mode
```
# Top functions by CPU
top
top10

# Show specific function
list functionName

# View as graph (requires graphviz)
web

# View as PDF
pdf

# Show call graph
traces

# Show cumulative time
top -cum

# Filter by function
top grep main
```

### Web UI
```bash
# Start web UI
go tool pprof -http=:8080 cpu.prof

# Browse to http://localhost:8080
# Interactive flame graph, top view, source view
```

## Execution Tracing

### Generate Trace
```bash
# During tests
go test -trace=trace.out

# In application
import "runtime/trace"

f, _ := os.Create("trace.out")
trace.Start(f)
defer trace.Stop()

# Or via HTTP
curl http://localhost:6060/debug/pprof/trace?seconds=5 > trace.out
```

### Analyze Trace
```bash
# Open trace viewer
go tool trace trace.out

# View in browser - shows:
# - Goroutine execution
# - Network blocking
# - Synchronization blocking
# - System calls
# - GC events
```

## Profiling in Code

### Basic CPU Profile
```go
import (
    "os"
    "runtime/pprof"
)

func main() {
    f, _ := os.Create("cpu.prof")
    defer f.Close()

    pprof.StartCPUProfile(f)
    defer pprof.StopCPUProfile()

    // Your code here
}
```

### Heap Profile
```go
import (
    "os"
    "runtime/pprof"
)

func writeHeapProfile() {
    f, _ := os.Create("heap.prof")
    defer f.Close()
    pprof.WriteHeapProfile(f)
}
```

### HTTP pprof Server
```go
import (
    "net/http"
    _ "net/http/pprof"
)

func main() {
    go func() {
        log.Println(http.ListenAndServe("localhost:6060", nil))
    }()

    // Your application code
}

// Access profiles at:
// http://localhost:6060/debug/pprof/
```

## Analyzing Results

### CPU Profile Interpretation
- **flat**: Time spent in function itself
- **cum**: Cumulative time (including callees)
- **%**: Percentage of total time

### Memory Profile Interpretation
- **alloc_space**: Total allocated bytes
- **alloc_objects**: Total allocated objects
- **inuse_space**: Currently in-use bytes
- **inuse_objects**: Currently in-use objects

### Finding Hot Paths
```bash
# Top CPU consumers
go tool pprof -top cpu.prof

# Top memory allocators
go tool pprof -top mem.prof

# Call graph
go tool pprof -web cpu.prof
```

## Benchmarking with Profiling

```bash
# CPU + memory profile
go test -bench=. -cpuprofile=cpu.prof -memprofile=mem.prof -benchmem

# Compare before/after
benchstat old.txt new.txt
```

## Continuous Profiling

### Datadog Example
```go
import "gopkg.in/DataDog/dd-trace-go.v1/profiler"

err := profiler.Start(
    profiler.WithService("myapp"),
    profiler.WithEnv("production"),
)
defer profiler.Stop()
```

## Common Optimizations

### Reduce Allocations
```go
// Before: Creates new slice each time
func process(data []int) []int {
    result := make([]int, 0)
    for _, v := range data {
        result = append(result, v*2)
    }
    return result
}

// After: Preallocate with capacity
func process(data []int) []int {
    result := make([]int, 0, len(data))
    for _, v := range data {
        result = append(result, v*2)
    }
    return result
}
```

### Use sync.Pool
```go
var bufferPool = sync.Pool{
    New: func() interface{} {
        return new(bytes.Buffer)
    },
}

func process() {
    buf := bufferPool.Get().(*bytes.Buffer)
    defer bufferPool.Put(buf)
    buf.Reset()

    // Use buffer
}
```

### Avoid String Concatenation in Loops
```go
// Slow
var s string
for _, item := range items {
    s += item  // Creates new string each time
}

// Fast
var b strings.Builder
for _, item := range items {
    b.WriteString(item)
}
s := b.String()
```

## Makefile Example

```makefile
.PHONY: profile-cpu
profile-cpu:
	go test -bench=. -cpuprofile=cpu.prof
	go tool pprof -http=:8080 cpu.prof

.PHONY: profile-mem
profile-mem:
	go test -bench=. -memprofile=mem.prof
	go tool pprof -http=:8080 mem.prof

.PHONY: trace
trace:
	go test -trace=trace.out
	go tool trace trace.out

.PHONY: profile-serve
profile-serve:
	@echo "Starting pprof server on :6060"
	@echo "CPU: curl http://localhost:6060/debug/pprof/profile?seconds=30 > cpu.prof"
	@echo "Heap: curl http://localhost:6060/debug/pprof/heap > heap.prof"
	go run -tags pprof main.go
```

## Best Practices

1. **Profile in production-like environment**: Real data, real load
2. **Focus on hot paths**: Optimize the 20% that matters
3. **Measure before and after**: Use benchstat for comparisons
4. **Profile different aspects**: CPU, memory, blocking, contention
5. **Use flame graphs**: Visual representation is powerful
6. **Sample for sufficient time**: 30+ seconds for CPU profiles
7. **Check GC impact**: High GC time indicates memory issues
8. **Monitor in production**: Continuous profiling catches regressions

## Resources

- [pprof Package](https://pkg.go.dev/net/http/pprof)
- [Profiling Go Programs](https://go.dev/blog/pprof)
- [Execution Tracer](https://go.dev/blog/execution-tracer)
- [Diagnostics](https://go.dev/doc/diagnostics)
