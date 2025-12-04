# Benchmark Workflow

Run Go benchmarks to measure and optimize performance.

## When to Use

- "benchmark go code"
- "measure performance"
- "run benchmarks"
- "profile performance"
- "compare benchmark results"

## Quick Commands

### Basic Benchmarking
```bash
# Run all benchmarks
go test -bench=.

# Run benchmarks in specific package
go test -bench=. ./internal/handlers

# Run specific benchmark
go test -bench=BenchmarkMyFunction

# Run benchmarks matching pattern
go test -bench=BenchmarkUser.*
```

### With Memory Statistics
```bash
# Show memory allocations
go test -bench=. -benchmem

# Example output:
# BenchmarkFunction-8   1000000    1234 ns/op    512 B/op    5 allocs/op
#                       ^^^^^^^^^  ^^^^^^^^^^^   ^^^^^^^^^   ^^^^^^^^^^^^^
#                       iterations  ns/operation  bytes/op    allocations/op
```

### Benchmark Time Control
```bash
# Run for specific duration (default 1s)
go test -bench=. -benchtime=10s

# Run specific number of iterations
go test -bench=. -benchtime=1000x

# Quick benchmark (shorter duration)
go test -bench=. -benchtime=100ms
```

## Writing Benchmarks

### Basic Benchmark
```go
func BenchmarkMyFunction(b *testing.B) {
    // Setup (not timed)
    input := generateInput()

    // Reset timer to exclude setup
    b.ResetTimer()

    // Run function b.N times
    for i := 0; i < b.N; i++ {
        myFunction(input)
    }
}
```

### Benchmark with Table Tests
```go
func BenchmarkParse(b *testing.B) {
    benchmarks := []struct {
        name  string
        input string
    }{
        {"small", "small input"},
        {"medium", "medium sized input string"},
        {"large", strings.Repeat("large ", 1000)},
    }

    for _, bm := range benchmarks {
        b.Run(bm.name, func(b *testing.B) {
            for i := 0; i < b.N; i++ {
                parse(bm.input)
            }
        })
    }
}
```

### Parallel Benchmarks
```go
func BenchmarkParallel(b *testing.B) {
    b.RunParallel(func(pb *testing.PB) {
        for pb.Next() {
            // Code to benchmark
            doWork()
        }
    })
}
```

### Reporting Custom Metrics
```go
func BenchmarkCustomMetrics(b *testing.B) {
    var totalBytes int64

    for i := 0; i < b.N; i++ {
        data := processData()
        totalBytes += int64(len(data))
    }

    // Report custom metric
    b.ReportMetric(float64(totalBytes)/float64(b.N), "bytes/op")
}
```

## Profiling with Benchmarks

### CPU Profiling
```bash
# Generate CPU profile
go test -bench=. -cpuprofile=cpu.prof

# Analyze CPU profile
go tool pprof cpu.prof

# Interactive pprof commands:
# top10       - Show top 10 functions
# list Func   - Show source for function
# web         - Generate graph (requires graphviz)
# pdf         - Generate PDF (requires graphviz)

# Generate CPU profile visualization
go tool pprof -http=:8080 cpu.prof
```

### Memory Profiling
```bash
# Generate memory profile
go test -bench=. -memprofile=mem.prof

# Analyze memory profile
go tool pprof mem.prof

# Show allocations
go tool pprof -alloc_space mem.prof

# Show in-use memory
go tool pprof -inuse_space mem.prof
```

### Block Profiling
```bash
# Profile blocking operations
go test -bench=. -blockprofile=block.prof

# Analyze
go tool pprof block.prof
```

### Mutex Profiling
```bash
# Profile mutex contention
go test -bench=. -mutexprofile=mutex.prof

# Analyze
go tool pprof mutex.prof
```

## Comparing Benchmarks

### Using benchstat
```bash
# Install benchstat
go install golang.org/x/perf/cmd/benchstat@latest

# Run baseline benchmark
go test -bench=. -count=10 > old.txt

# Make changes and run new benchmark
go test -bench=. -count=10 > new.txt

# Compare results
benchstat old.txt new.txt

# Example output:
# name        old time/op  new time/op  delta
# Function-8  123ns ± 2%   98ns ± 1%   -20.33% (p=0.000 n=10+10)
```

### Manual Comparison
```bash
# Run benchmark multiple times for stability
go test -bench=BenchmarkMyFunc -count=5

# Save to file
go test -bench=. -benchmem > benchmark.txt

# Compare manually
# name                time/op    mem/op    allocs/op
# Before: 1234ns     512B       5
# After:  987ns      256B       3
# Delta:  -20%       -50%       -40%
```

## Advanced Techniques

### Sub-Benchmarks
```go
func BenchmarkComplexOperation(b *testing.B) {
    sizes := []int{10, 100, 1000, 10000}

    for _, size := range sizes {
        b.Run(fmt.Sprintf("size-%d", size), func(b *testing.B) {
            data := make([]int, size)
            b.ResetTimer()

            for i := 0; i < b.N; i++ {
                processSlice(data)
            }
        })
    }
}
```

### Stopping and Starting Timer
```go
func BenchmarkWithSetup(b *testing.B) {
    for i := 0; i < b.N; i++ {
        b.StopTimer()
        // Expensive setup not timed
        data := generateExpensiveData()
        b.StartTimer()

        // This is timed
        process(data)
    }
}
```

### Setting Bytes Processed
```go
func BenchmarkProcess(b *testing.B) {
    data := make([]byte, 1024*1024) // 1MB

    b.SetBytes(int64(len(data)))
    b.ResetTimer()

    for i := 0; i < b.N; i++ {
        process(data)
    }
    // Will report MB/s
}
```

## Common Patterns

### Benchmark HTTP Handler
```go
func BenchmarkHandler(b *testing.B) {
    handler := setupHandler()
    req := httptest.NewRequest("GET", "/users", nil)

    b.ResetTimer()

    for i := 0; i < b.N; i++ {
        w := httptest.NewRecorder()
        handler.ServeHTTP(w, req)
    }
}
```

### Benchmark with Pool
```go
var bufferPool = sync.Pool{
    New: func() interface{} {
        return new(bytes.Buffer)
    },
}

func BenchmarkWithPool(b *testing.B) {
    for i := 0; i < b.N; i++ {
        buf := bufferPool.Get().(*bytes.Buffer)
        buf.Reset()

        // Use buffer
        processWithBuffer(buf)

        bufferPool.Put(buf)
    }
}
```

### Benchmark Allocations
```go
func BenchmarkAllocations(b *testing.B) {
    b.ReportAllocs() // Equivalent to -benchmem

    for i := 0; i < b.N; i++ {
        // Code that allocates
        _ = make([]int, 100)
    }
}
```

## Makefile Example

```makefile
.PHONY: bench
bench:
	go test -bench=. -benchmem ./...

.PHONY: bench-cpu
bench-cpu:
	go test -bench=. -cpuprofile=cpu.prof
	go tool pprof -http=:8080 cpu.prof

.PHONY: bench-mem
bench-mem:
	go test -bench=. -memprofile=mem.prof
	go tool pprof -http=:8080 mem.prof

.PHONY: bench-compare
bench-compare:
	@echo "Running baseline benchmarks..."
	go test -bench=. -count=10 -benchmem > bench-old.txt
	@echo "Make your changes, then run: make bench-compare-new"

.PHONY: bench-compare-new
bench-compare-new:
	@echo "Running new benchmarks..."
	go test -bench=. -count=10 -benchmem > bench-new.txt
	benchstat bench-old.txt bench-new.txt
```

## Best Practices

1. **Run multiple iterations**: Use `-count=5` or higher for stable results
2. **Use benchstat**: For statistical comparison of results
3. **Reset timer after setup**: Exclude setup time with `b.ResetTimer()`
4. **Report allocations**: Always use `-benchmem` to see memory impact
5. **Benchmark real scenarios**: Use realistic data and workloads
6. **Run on consistent hardware**: Same machine, same load for comparisons
7. **Profile before optimizing**: Find bottlenecks with pprof
8. **Benchmark alternatives**: Compare different implementations
9. **Document baseline**: Save benchmark results in version control
10. **Use sub-benchmarks**: Test different input sizes and scenarios

## Interpreting Results

### Time/Operation
- Lower is better
- Represents average time per operation
- Compare before/after optimization

### Allocations/Operation
- Fewer is better
- Each allocation has overhead
- Reducing allocations improves performance

### Bytes/Operation
- Fewer is better
- Memory allocations cause GC pressure
- Optimize hot paths to reduce allocations

### MB/s (when using SetBytes)
- Higher is better
- Throughput measurement
- Useful for I/O operations

## Common Flags

| Flag | Purpose |
|------|---------|
| `-bench=.` | Run all benchmarks |
| `-benchmem` | Show memory stats |
| `-benchtime=Xs` | Run for X seconds |
| `-benchtime=Nx` | Run N iterations |
| `-count=N` | Run benchmarks N times |
| `-cpuprofile=file` | Write CPU profile |
| `-memprofile=file` | Write memory profile |
| `-blockprofile=file` | Write block profile |
| `-mutexprofile=file` | Write mutex profile |

## Resources

- [Benchmarking in Go](https://pkg.go.dev/testing#hdr-Benchmarks)
- [benchstat Tool](https://pkg.go.dev/golang.org/x/perf/cmd/benchstat)
- [pprof Tool](https://github.com/google/pprof)
- [Go Performance](https://go.dev/doc/diagnostics#profiling)
