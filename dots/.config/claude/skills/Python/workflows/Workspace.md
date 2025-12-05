# Workspace Workflow

Manage multiple Python packages in a monorepo with uv workspaces.

## When to Use

- "python monorepo"
- "multiple packages"
- "workspace"
- "manage related packages"

## Quick Setup

### Create Workspace Structure

```
myworkspace/
├── pyproject.toml          # Workspace root
├── packages/
│   ├── core/
│   │   ├── pyproject.toml
│   │   └── src/core/
│   ├── api/
│   │   ├── pyproject.toml
│   │   └── src/api/
│   └── cli/
│       ├── pyproject.toml
│       └── src/cli/
└── uv.lock                 # Shared lock file
```

### Root pyproject.toml

```toml
[tool.uv.workspace]
members = ["packages/*"]

[tool.uv.sources]
# Workspace dependencies
core = { workspace = true }
api = { workspace = true }
cli = { workspace = true }
```

### Package pyproject.toml

```toml
# packages/api/pyproject.toml
[project]
name = "myworkspace-api"
version = "0.1.0"
dependencies = [
    "fastapi>=0.104.0",
]

[tool.uv.sources]
core = { workspace = true }  # Depend on workspace package
```

## Commands

```bash
# Sync all workspace packages
uv sync

# Build specific package
cd packages/api
uv build

# Run from specific package
cd packages/cli
uv run myworkspace-cli

# Add dependency to specific package
cd packages/api
uv add httpx
```

## Benefits

1. **Single lock file** - Consistent versions across packages
2. **Shared dependencies** - Avoid duplication
3. **Local development** - Easy cross-package development
4. **Atomic updates** - Update all packages together

## Best Practices

1. **Shared version** - Use same Python version
2. **Consistent naming** - `workspace-package` pattern
3. **Clear boundaries** - Each package has clear purpose
4. **Minimal coupling** - Avoid circular dependencies
