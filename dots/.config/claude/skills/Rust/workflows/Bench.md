# Benchmark Workflow

Benchmark Rust code with Criterion.

## When to Use

- "benchmark"
- "cargo bench"
- "performance test"
- "criterion"

## Setup

```toml
[dev-dependencies]
criterion = { version = "0.7", features = ["html_reports"] }

[[bench]]
name = "my_benchmark"
harness = false
```

## Quick Commands

```bash
# Run benchmarks
cargo bench

# Run specific benchmark
cargo bench my_function

# Save baseline
cargo bench -- --save-baseline main

# Compare to baseline
cargo bench -- --baseline main
```

## Basic Benchmark

```rust
// benches/my_benchmark.rs
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn fibonacci(n: u64) -> u64 {
    match n {
        0 => 1,
        1 => 1,
        n => fibonacci(n-1) + fibonacci(n-2),
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fib 20", |b| b.iter(|| fibonacci(black_box(20))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
```

## Parametrized Benchmarks

```rust
use criterion::{BenchmarkId, Criterion};

fn bench_sizes(c: &mut Criterion) {
    let mut group = c.benchmark_group("sizes");

    for size in [10, 100, 1000].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(size),
            size,
            |b, &size| {
                b.iter(|| process_data(black_box(size)))
            },
        );
    }

    group.finish();
}
```

## Comparing Functions

```rust
fn bench_comparison(c: &mut Criterion) {
    let mut group = c.benchmark_group("algorithms");

    group.bench_function("algorithm_a", |b| {
        b.iter(|| algorithm_a(black_box(&data)))
    });

    group.bench_function("algorithm_b", |b| {
        b.iter(|| algorithm_b(black_box(&data)))
    });

    group.finish();
}
```

## Best Practices

1. **Use `black_box`**: Prevent compiler optimization
2. **Warmup iterations**: Let CPU caches warm up
3. **Save baselines**: Compare against previous runs
4. **Statistical rigor**: Criterion provides this
5. **Profile first**: Use profilers before micro-benchmarks
