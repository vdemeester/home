# Generate Workflow

Code generation with go generate, mocking, and other generation tools.

## When to Use

- "generate go code"
- "run go generate"
- "create mocks"
- "generate from protobuf"
- "use stringer"

## Quick Commands

### go generate
```bash
# Run generators in current package
go generate

# Run in all packages
go generate ./...

# Verbose output
go generate -v ./...

# Dry run
go generate -n ./...

# Run specific generator
go generate -run mockgen ./...
```

## Common Generators

### Stringer (Enum String Methods)
```go
//go:generate stringer -type=Status
type Status int

const (
    Pending Status = iota
    Active
    Completed
)

// Generates: status_string.go with String() method
```

```bash
# Install
go install golang.org/x/tools/cmd/stringer@latest

# Generate
go generate ./...

# Usage
s := Active
fmt.Println(s.String()) // "Active"
```

### mockgen (Mock Interfaces)
```go
//go:generate mockgen -destination=mocks/mock_database.go -package=mocks . Database

type Database interface {
    Get(id string) (*User, error)
    Save(user *User) error
}
```

```bash
# Install
go install github.com/golang/mock/mockgen@latest

# Generate mocks
go generate ./...

# Use in tests
mockDB := mocks.NewMockDatabase(ctrl)
mockDB.EXPECT().Get("123").Return(user, nil)
```

### protoc (Protocol Buffers)
```proto
// user.proto
syntax = "proto3";
package api;

message User {
    string id = 1;
    string name = 2;
}
```

```go
//go:generate protoc --go_out=. --go_opt=paths=source_relative user.proto
```

```bash
# Install
go install google.golang.org/protobuf/cmd/protoc-gen-go@latest

# Generate
go generate ./...
```

### Embed (Static Files)
```go
//go:embed templates/*.html
var templates embed.FS

//go:embed static/*
var static embed.FS

func main() {
    data, _ := templates.ReadFile("templates/index.html")
}
```

## Advanced Generators

### sqlc (SQL to Go)
```yaml
# sqlc.yaml
version: "2"
sql:
  - schema: "schema.sql"
    queries: "queries.sql"
    engine: "postgresql"
    gen:
      go:
        package: "db"
        out: "db"
```

```go
//go:generate sqlc generate
```

### Wire (Dependency Injection)
```go
//go:build wireinject

//go:generate wire

func InitializeApp() (*App, error) {
    wire.Build(
        NewDatabase,
        NewUserService,
        NewApp,
    )
    return nil, nil
}
```

### oapi-codegen (OpenAPI)
```go
//go:generate oapi-codegen -package api -generate types,server openapi.yaml > api.gen.go
```

## Generator Directives

### Syntax
```go
//go:generate command arg1 arg2
```

### Examples
```go
// Generate stringer
//go:generate stringer -type=Color

// Multiple commands
//go:generate mockgen -destination=mocks/mock.go . Interface
//go:generate gofmt -w mocks/mock.go

// With environment variables
//go:generate sh -c "VERSION=$(git describe) envsubst < version.tmpl > version.go"

// Conditional generation
//go:generate echo "Generating..."
```

## Makefile Integration

```makefile
.PHONY: generate
generate:
	go generate ./...
	gofmt -w .

.PHONY: generate-mocks
generate-mocks:
	go generate -run mockgen ./...

.PHONY: generate-proto
generate-proto:
	protoc --go_out=. --go_opt=paths=source_relative api/*.proto

.PHONY: generate-check
generate-check:
	go generate ./...
	git diff --exit-code

.PHONY: clean-generated
clean-generated:
	find . -name "*.gen.go" -delete
	find . -name "*_string.go" -delete
```

## Best Practices

1. **Commit generated code**: Makes builds reproducible
2. **Run before building**: Ensure generated code is current
3. **Use specific versions**: Pin generator tool versions
4. **Document generators**: Comment what each directive does
5. **Format generated code**: Run gofmt after generation
6. **Check in CI**: Verify generated code is up-to-date
7. **Separate generated files**: Use .gen.go suffix

## CI/CD Integration

### GitHub Actions
```yaml
- name: Install generators
  run: |
    go install golang.org/x/tools/cmd/stringer@latest
    go install github.com/golang/mock/mockgen@latest

- name: Run generators
  run: go generate ./...

- name: Check for changes
  run: |
    if [[ `git status --porcelain` ]]; then
      echo "Generated code is out of date"
      git diff
      exit 1
    fi
```

## Common Patterns

### Version Embedding
```go
//go:generate sh -c "echo 'package main\n\nconst Version = \"'$(git describe --tags)'\"' > version.go"

func main() {
    fmt.Println("Version:", Version)
}
```

### Interface Verification
```go
//go:generate go run check_interfaces.go

// Verify implementation
var _ http.Handler = (*MyHandler)(nil)
```

### Template Expansion
```go
//go:generate go run gen_template.go

// gen_template.go
package main

import "text/template"

const tmpl = `// Code generated. DO NOT EDIT.
package {{.Package}}

type {{.Name}} struct {}`

func main() {
    // Generate code from template
}
```

## Resources

- [go generate](https://go.dev/blog/generate)
- [stringer](https://pkg.go.dev/golang.org/x/tools/cmd/stringer)
- [mockgen](https://github.com/golang/mock)
- [embed](https://pkg.go.dev/embed)
- [protobuf](https://protobuf.dev/)
