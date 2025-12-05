# Lint Workflow

Lint and format Python code with ruff - the fast, modern linter and formatter.

## When to Use

- "lint python code"
- "format python"
- "ruff check"
- "ruff format"
- "fix code style"

## Quick Commands

### Linting

```bash
# Check for issues
uv run ruff check .

# Auto-fix issues
uv run ruff check --fix .

# Show fixes that would be applied (dry run)
uv run ruff check --fix --diff .

# Check specific file
uv run ruff check src/myproject/main.py

# Output as JSON
uv run ruff check --output-format=json .
```

### Formatting

```bash
# Format code
uv run ruff format .

# Check if code would be reformatted (dry run)
uv run ruff format --check .

# Show diff of what would change
uv run ruff format --diff .

# Format specific file
uv run ruff format src/myproject/main.py
```

### Combined

```bash
# Lint and format in one go
uv run ruff check --fix . && uv run ruff format .
```

## Configuration

### pyproject.toml

```toml
[tool.ruff]
# Set line length (default: 88, Black compatible)
line-length = 100

# Target Python version
target-version = "py311"

# Source code directories
src = ["src", "tests"]

# Files to exclude
extend-exclude = [
    ".git",
    "__pycache__",
    "dist",
    "build",
    ".venv",
]

[tool.ruff.lint]
# Enable rule sets
select = [
    "E",      # pycodestyle errors
    "W",      # pycodestyle warnings
    "F",      # Pyflakes
    "I",      # isort
    "N",      # pep8-naming
    "UP",     # pyupgrade
    "ANN",    # flake8-annotations
    "ASYNC",  # flake8-async
    "S",      # flake8-bandit
    "B",      # flake8-bugbear
    "A",      # flake8-builtins
    "C4",     # flake8-comprehensions
    "DTZ",    # flake8-datetimez
    "T10",    # flake8-debugger
    "EM",     # flake8-errmsg
    "ISC",    # flake8-implicit-str-concat
    "ICN",    # flake8-import-conventions
    "G",      # flake8-logging-format
    "PIE",    # flake8-pie
    "T20",    # flake8-print
    "PT",     # flake8-pytest-style
    "Q",      # flake8-quotes
    "RSE",    # flake8-raise
    "RET",    # flake8-return
    "SIM",    # flake8-simplify
    "TID",    # flake8-tidy-imports
    "ARG",    # flake8-unused-arguments
    "PTH",    # flake8-use-pathlib
    "PL",     # Pylint
    "TRY",    # tryceratops
    "RUF",    # Ruff-specific rules
]

# Rules to ignore
ignore = [
    "ANN101",  # Missing type annotation for self
    "ANN102",  # Missing type annotation for cls
    "D203",    # 1 blank line required before class docstring
    "D213",    # Multi-line docstring summary should start at the second line
]

# Allow fix for all enabled rules (when `--fix` is used)
fixable = ["ALL"]
unfixable = []

# Per-file ignores
[tool.ruff.lint.per-file-ignores]
"tests/*" = [
    "S101",    # Use of assert
    "ARG",     # Unused arguments
    "PLR2004", # Magic value used in comparison
]
"__init__.py" = [
    "F401",    # Unused imports
]

# isort settings
[tool.ruff.lint.isort]
known-first-party = ["myproject"]
force-single-line = false
lines-after-imports = 2

# flake8-quotes settings
[tool.ruff.lint.flake8-quotes]
docstring-quotes = "double"
inline-quotes = "double"

# pylint settings
[tool.ruff.lint.pylint]
max-args = 5
max-branches = 12
max-returns = 6
max-statements = 50
```

## Common Rules

### Import Organization (I)
```python
# Ruff automatically organizes imports
import os
import sys
from pathlib import Path

import requests
import httpx

from myproject import utils
from myproject.core import MyClass
```

### Type Annotations (ANN)
```python
# Enforces type hints
def greet(name: str) -> str:
    return f"Hello, {name}!"

def process_data(items: list[int]) -> dict[str, int]:
    return {"count": len(items), "sum": sum(items)}
```

### Pathlib Usage (PTH)
```python
# Bad: Using os.path
import os
path = os.path.join("data", "file.txt")

# Good: Using pathlib
from pathlib import Path
path = Path("data") / "file.txt"
```

### Simplifications (SIM)
```python
# Bad: Unnecessary comprehension
squares = [x ** 2 for x in numbers if x ** 2 > 10]

# Good: Direct filtering
squares = [x ** 2 for x in numbers if x > 3]
```

## Ignoring Rules

### Inline Ignore

```python
# Ignore specific rule for one line
x = eval(user_input)  # noqa: S307

# Ignore multiple rules
result = func()  # noqa: ARG001, ANN201

# Ignore all rules (use sparingly!)
dangerous_code()  # noqa
```

### File-level Ignore

```python
# At top of file
# ruff: noqa: ANN
```

## Integration with Pre-commit

```yaml
# .pre-commit-config.yaml
repos:
  - repo: https://github.com/astral-sh/ruff-pre-commit
    rev: v0.8.0
    hooks:
      # Run linter
      - id: ruff
        args: [--fix]
      # Run formatter
      - id: ruff-format
```

## CI/CD Integration

```yaml
# .github/workflows/lint.yml
name: Lint

on: [push, pull_request]

jobs:
  lint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: astral-sh/setup-uv@v1
      - name: Lint
        run: uv run ruff check .
      - name: Format check
        run: uv run ruff format --check .
```

## Best Practices

1. **Run ruff before committing** - Use pre-commit hooks
2. **Fix automatically** - Use `--fix` flag liberally
3. **Configure in pyproject.toml** - Keep all config in one place
4. **Use per-file ignores** - Different rules for tests vs src
5. **Don't ignore too much** - Only ignore rules when necessary
6. **Format before linting** - Format first, then lint
7. **CI enforcement** - Run in CI to catch issues
8. **Editor integration** - Configure your editor to run ruff on save
