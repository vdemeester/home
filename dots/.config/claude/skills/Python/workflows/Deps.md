# Dependencies Workflow

Manage Python dependencies with uv.

## When to Use

- "add python dependency"
- "update dependencies"
- "manage requirements"
- "uv lock"

## Quick Commands

```bash
# Add dependency
uv add requests

# Add dev dependency
uv add --dev pytest

# Add with version constraint
uv add 'requests>=2.31.0,<3.0.0'

# Remove dependency
uv remove requests

# Update all dependencies
uv lock --upgrade

# Sync environment with lock file
uv sync

# Export to requirements.txt
uv pip compile pyproject.toml -o requirements.txt
```

## Managing Dependencies

### Add Dependencies

```bash
# Runtime dependency
uv add httpx pydantic rich

# Development tools
uv add --dev pytest ruff mypy pytest-cov

# Optional dependency group
uv add --optional docs sphinx

# From git
uv add git+https://github.com/user/repo.git

# From git with branch/tag
uv add git+https://github.com/user/repo.git@main
uv add git+https://github.com/user/repo.git@v1.0.0

# From local path (editable)
uv add --editable ./local-package
```

### Update Dependencies

```bash
# Update specific package
uv lock --upgrade-package requests

# Update all packages
uv lock --upgrade

# Update and sync
uv lock --upgrade && uv sync
```

## Lock File

The `uv.lock` file ensures reproducible builds:

```bash
# Generate lock file
uv lock

# Sync from lock file
uv sync

# Include optional dependencies
uv sync --extra docs

# Include all optional dependencies
uv sync --all-extras
```

**Always commit `uv.lock` to version control.**

## Dependency Groups

### pyproject.toml

```toml
[project]
dependencies = [
    "requests>=2.31.0",
    "pydantic>=2.0.0",
]

[project.optional-dependencies]
dev = [
    "pytest>=8.0.0",
    "ruff>=0.8.0",
    "mypy>=1.13.0",
]
docs = [
    "sphinx>=7.0.0",
    "sphinx-rtd-theme>=2.0.0",
]
```

### Install Groups

```bash
# Install with dev dependencies
uv sync --group dev

# Install with docs dependencies
uv sync --extra docs

# Install all groups
uv sync --all-extras
```

## Best Practices

1. **Lock dependencies** - Always maintain `uv.lock`
2. **Pin versions** - Use version constraints
3. **Separate dev deps** - Use `--dev` for tools
4. **Regular updates** - Update dependencies monthly
5. **Security audit** - Check for vulnerabilities
6. **Minimal deps** - Only add what you need
