# Build Workflow

Build Go applications with proper flags, cross-compilation, and optimization.

## When to Use

- "build go project"
- "compile go binary"
- "cross compile"
- "build for linux/windows/mac"

## Quick Commands

### Basic Build
```bash
# Build current package
go build

# Build with output name
go build -o myapp

# Build specific package
go build ./cmd/myapp

# Build and install to $GOPATH/bin
go install ./cmd/myapp
```

### Cross-Compilation
```bash
# Build for Linux (from any OS)
GOOS=linux GOARCH=amd64 go build -o myapp-linux

# Build for Windows
GOOS=windows GOARCH=amd64 go build -o myapp.exe

# Build for macOS (Intel)
GOOS=darwin GOARCH=amd64 go build -o myapp-darwin-amd64

# Build for macOS (Apple Silicon)
GOOS=darwin GOARCH=arm64 go build -o myapp-darwin-arm64

# Build for ARM (Raspberry Pi)
GOOS=linux GOARCH=arm GOARM=7 go build -o myapp-arm
```

### Common GOOS/GOARCH Combinations
| Target | GOOS | GOARCH |
|--------|------|--------|
| Linux 64-bit | linux | amd64 |
| Linux 32-bit | linux | 386 |
| Linux ARM | linux | arm64 |
| Windows 64-bit | windows | amd64 |
| macOS Intel | darwin | amd64 |
| macOS Apple Silicon | darwin | arm64 |
| FreeBSD | freebsd | amd64 |

## Build Flags

### Optimization Flags
```bash
# Reduce binary size (strip debug info)
go build -ldflags="-s -w" -o myapp

# Add version information
go build -ldflags="-X main.version=1.0.0 -X main.commit=$(git rev-parse HEAD)" -o myapp
```

**Common ldflags:**
- `-s` - Omit symbol table and debug info
- `-w` - Omit DWARF symbol table
- `-X` - Set string variable value

### Build Tags
```bash
# Build with specific tags
go build -tags=prod

# Multiple tags
go build -tags="prod,mysql"

# Build without CGO
CGO_ENABLED=0 go build
```

### Race Detection
```bash
# Build with race detector (testing only, slower)
go build -race
```

### Trimpath for Reproducible Builds
```bash
# Remove absolute paths from binary
go build -trimpath
```

## Build Modes

### Static Binary (No Dependencies)
```bash
# Build completely static binary
CGO_ENABLED=0 GOOS=linux go build -a -ldflags="-s -w" -o myapp
```

**Flags explained:**
- `CGO_ENABLED=0` - Disable CGO (C dependencies)
- `-a` - Force rebuild of all packages
- `-ldflags="-s -w"` - Strip debug info

### Shared Library
```bash
# Build as shared library (.so)
go build -buildmode=c-shared -o mylib.so
```

### Plugin
```bash
# Build as plugin
go build -buildmode=plugin -o myplugin.so
```

## Common Patterns

### Multi-Binary Project
```bash
# Build all binaries in cmd/
for dir in cmd/*/; do
    binary=$(basename "$dir")
    go build -o "bin/$binary" "./$dir"
done
```

### Build Script with Version
```bash
#!/bin/bash
VERSION=$(git describe --tags --always --dirty)
COMMIT=$(git rev-parse --short HEAD)
DATE=$(date -u +%Y-%m-%dT%H:%M:%SZ)

go build -ldflags="-X main.version=${VERSION} \
                    -X main.commit=${COMMIT} \
                    -X main.date=${DATE}" \
         -o myapp
```

### Embed Version in Code
```go
// main.go
package main

var (
    version = "dev"
    commit  = "unknown"
    date    = "unknown"
)

func main() {
    fmt.Printf("Version: %s\nCommit: %s\nBuilt: %s\n", version, commit, date)
}
```

## Makefile Example

```makefile
BINARY_NAME=myapp
VERSION=$(shell git describe --tags --always --dirty)
COMMIT=$(shell git rev-parse --short HEAD)
DATE=$(shell date -u +%Y-%m-%dT%H:%M:%SZ)

LDFLAGS=-ldflags="-s -w -X main.version=${VERSION} -X main.commit=${COMMIT} -X main.date=${DATE}"

.PHONY: build
build:
	go build ${LDFLAGS} -o ${BINARY_NAME}

.PHONY: build-all
build-all:
	GOOS=linux GOARCH=amd64 go build ${LDFLAGS} -o ${BINARY_NAME}-linux-amd64
	GOOS=darwin GOARCH=amd64 go build ${LDFLAGS} -o ${BINARY_NAME}-darwin-amd64
	GOOS=darwin GOARCH=arm64 go build ${LDFLAGS} -o ${BINARY_NAME}-darwin-arm64
	GOOS=windows GOARCH=amd64 go build ${LDFLAGS} -o ${BINARY_NAME}-windows-amd64.exe

.PHONY: clean
clean:
	rm -f ${BINARY_NAME}*
```

## Troubleshooting

### CGO Issues
```bash
# If you get CGO errors, disable it
CGO_ENABLED=0 go build

# Or install required C libraries
# For SQLite on Ubuntu/Debian:
sudo apt-get install gcc libsqlite3-dev
```

### Module Issues
```bash
# Ensure modules are downloaded
go mod download

# Verify modules
go mod verify

# Clean module cache
go clean -modcache
```

### Build Cache
```bash
# Clean build cache
go clean -cache

# Build without cache
go build -a
```

## Best Practices

1. **Always use `go.mod`**: Don't use `GOPATH` mode
2. **Pin versions**: Use `go.mod` to lock dependency versions
3. **Use `-trimpath`**: For reproducible builds
4. **Static binaries for containers**: Use `CGO_ENABLED=0` for Docker
5. **Version your binaries**: Embed version info with `-ldflags`
6. **Test cross-compilation**: Ensure it works for all target platforms
7. **Optimize for production**: Use `-ldflags="-s -w"` to reduce size

## Integration with CI/CD

### GitHub Actions Example
```yaml
- name: Build
  run: |
    go build -ldflags="-s -w" -o myapp

- name: Build for multiple platforms
  run: |
    GOOS=linux GOARCH=amd64 go build -o myapp-linux
    GOOS=darwin GOARCH=amd64 go build -o myapp-darwin
    GOOS=windows GOARCH=amd64 go build -o myapp.exe
```

## Resources

- [Go Build Command](https://pkg.go.dev/cmd/go#hdr-Compile_packages_and_dependencies)
- [Build Modes](https://pkg.go.dev/cmd/go#hdr-Build_modes)
- [Cross Compilation](https://go.dev/doc/install/source#environment)
