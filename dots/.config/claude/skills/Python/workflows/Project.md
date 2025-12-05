# Project Workflow

Create and manage Python projects with uv, following modern best practices.

## When to Use

- "create python project"
- "new python project"
- "initialize python app"
- "uv init"
- "setup python package"

## Quick Start

### Create New Project

```bash
# Create new project with default structure
uv init myproject

# Create library project
uv init --lib myproject

# Create app project (default)
uv init --app myproject

# Create in existing directory
cd myproject
uv init
```

### Project Structure Created

```
myproject/
├── .python-version          # Python version (e.g., 3.11)
├── README.md
├── pyproject.toml          # Project configuration
├── hello.py                # Entry point (for apps)
└── src/
    └── myproject/
        └── __init__.py     # For libraries
```

## Configure Project

### Edit pyproject.toml

```toml
[project]
name = "myproject"
version = "0.1.0"
description = "Description of my awesome project"
readme = "README.md"
requires-python = ">=3.11"
authors = [
    { name = "Your Name", email = "your.email@example.com" }
]
license = { text = "MIT" }
classifiers = [
    "Development Status :: 3 - Alpha",
    "Intended Audience :: Developers",
    "Programming Language :: Python :: 3.11",
]

dependencies = [
    # Runtime dependencies
]

[project.optional-dependencies]
dev = [
    "pytest>=8.0.0",
    "pytest-cov>=6.0.0",
    "ruff>=0.8.0",
    "mypy>=1.13.0",
]

[project.scripts]
myproject = "myproject.main:main"

[build-system]
requires = ["hatchling"]
build-backend = "hatchling.build"

# Ruff configuration
[tool.ruff]
line-length = 100
target-version = "py311"
src = ["src", "tests"]

[tool.ruff.lint]
select = [
    "E",   # pycodestyle errors
    "W",   # pycodestyle warnings
    "F",   # pyflakes
    "I",   # isort
    "N",   # pep8-naming
    "B",   # flake8-bugbear
    "Q",   # flake8-quotes
    "UP",  # pyupgrade
    "ANN", # flake8-annotations
    "S",   # flake8-bandit
    "C4",  # flake8-comprehensions
]
ignore = [
    "ANN101",  # Missing type annotation for self
    "ANN102",  # Missing type annotation for cls
]

[tool.ruff.lint.per-file-ignores]
"tests/*" = ["S101"]  # Allow assert in tests

# mypy configuration
[tool.mypy]
python_version = "3.11"
strict = true
warn_return_any = true
warn_unused_configs = true
disallow_untyped_defs = true
disallow_any_generics = true
check_untyped_defs = true
no_implicit_optional = true
warn_redundant_casts = true
warn_unused_ignores = true

# pytest configuration
[tool.pytest.ini_options]
testpaths = ["tests"]
python_files = "test_*.py"
python_functions = "test_*"
addopts = [
    "--strict-markers",
    "--strict-config",
    "--cov=src",
    "--cov-report=term-missing",
    "--cov-report=html",
]

# Coverage configuration
[tool.coverage.run]
source = ["src"]
omit = ["tests/*", "**/__pycache__/*"]

[tool.coverage.report]
precision = 2
show_missing = true
skip_covered = false
exclude_lines = [
    "pragma: no cover",
    "def __repr__",
    "raise AssertionError",
    "raise NotImplementedError",
    "if __name__ == .__main__.:",
    "if TYPE_CHECKING:",
]
```

## Add Dependencies

### Runtime Dependencies

```bash
# Add single package
uv add requests

# Add multiple packages
uv add requests httpx rich

# Add with version constraint
uv add 'requests>=2.31.0,<3.0.0'

# Add from git
uv add git+https://github.com/user/repo.git

# Add from local path
uv add --editable ./local-package
```

### Development Dependencies

```bash
# Add dev dependencies
uv add --dev pytest pytest-cov ruff mypy

# Add testing tools
uv add --dev pytest pytest-asyncio pytest-mock

# Add type stubs
uv add --dev types-requests types-pyyaml
```

### Optional Dependencies

```bash
# Add optional dependency group
uv add --optional docs sphinx sphinx-rtd-theme

# Install with optional group
uv sync --extra docs
```

## Project Setup Workflow

### 1. Initialize Project

```bash
uv init myproject
cd myproject
```

### 2. Set Python Version

```bash
# Use specific Python version
echo "3.11" > .python-version

# Or let uv detect/install
uv python pin 3.11
```

### 3. Add Dependencies

```bash
# Add core dependencies
uv add requests pydantic

# Add development tools
uv add --dev pytest ruff mypy pytest-cov
```

### 4. Create Project Structure

```bash
# For applications
mkdir -p src/myproject tests docs
touch src/myproject/{__init__.py,main.py}
touch tests/{__init__.py,test_main.py}

# For libraries
mkdir -p src/myproject tests docs examples
touch src/myproject/{__init__.py,core.py}
touch tests/{__init__.py,test_core.py}
touch examples/basic_usage.py
```

### 5. Initialize Git

```bash
git init
cat > .gitignore << 'EOF'
# Python
__pycache__/
*.py[cod]
*$py.class
*.so
.Python
env/
venv/
.venv/
ENV/
build/
develop-eggs/
dist/
downloads/
eggs/
.eggs/
lib/
lib64/
parts/
sdist/
var/
wheels/
*.egg-info/
.installed.cfg
*.egg

# Testing
.pytest_cache/
.coverage
htmlcov/
.tox/

# Type checking
.mypy_cache/
.dmypy.json

# IDEs
.vscode/
.idea/
*.swp
*.swo
*~

# uv
uv.lock
EOF

git add .
git commit -m "Initial commit"
```

### 6. Setup Pre-commit (Optional)

```bash
# Add pre-commit
uv add --dev pre-commit

# Create .pre-commit-config.yaml
cat > .pre-commit-config.yaml << 'EOF'
repos:
  - repo: https://github.com/astral-sh/ruff-pre-commit
    rev: v0.8.0
    hooks:
      - id: ruff
        args: [--fix]
      - id: ruff-format

  - repo: https://github.com/pre-commit/mirrors-mypy
    rev: v1.13.0
    hooks:
      - id: mypy
        additional_dependencies: []
EOF

# Install hooks
uv run pre-commit install
```

## Project Templates

### CLI Application

```python
# src/myproject/main.py
"""Main entry point for myproject CLI."""
import argparse
import sys
from typing import Sequence

def main(argv: Sequence[str] | None = None) -> int:
    """Run the CLI application.

    Args:
        argv: Command line arguments (defaults to sys.argv)

    Returns:
        Exit code (0 for success, non-zero for errors)
    """
    parser = argparse.ArgumentParser(description="My awesome CLI")
    parser.add_argument("--version", action="version", version="0.1.0")
    parser.add_argument("input", help="Input file path")
    parser.add_argument("-o", "--output", help="Output file path")

    args = parser.parse_args(argv)

    try:
        # Your logic here
        print(f"Processing {args.input}")
        return 0
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1

if __name__ == "__main__":
    sys.exit(main())
```

### Library Package

```python
# src/myproject/__init__.py
"""MyProject - A Python library for awesome things."""
from myproject.core import MyClass, my_function

__version__ = "0.1.0"
__all__ = ["MyClass", "my_function"]

# src/myproject/core.py
"""Core functionality for myproject."""
from typing import Any

class MyClass:
    """Main class for myproject."""

    def __init__(self, name: str) -> None:
        """Initialize MyClass.

        Args:
            name: Name of the instance
        """
        self.name = name

    def process(self, data: dict[str, Any]) -> dict[str, Any]:
        """Process data.

        Args:
            data: Input data dictionary

        Returns:
            Processed data dictionary
        """
        return {"name": self.name, **data}

def my_function(value: int) -> int:
    """Perform calculation on value.

    Args:
        value: Input integer

    Returns:
        Calculated result
    """
    return value * 2
```

## Run Project

### Development

```bash
# Run module
uv run python -m myproject

# Run script entry point
uv run myproject

# Run tests
uv run pytest

# Run with coverage
uv run pytest --cov

# Lint and format
uv run ruff check .
uv run ruff format .

# Type check
uv run mypy .
```

### Build and Install

```bash
# Build distribution
uv build

# Install locally
uv pip install -e .

# Install from built wheel
uv pip install dist/*.whl
```

## Common Project Commands

```bash
# Show project info
uv tree                    # Show dependency tree
cat pyproject.toml         # View configuration

# Update dependencies
uv lock --upgrade          # Update lock file
uv sync                    # Sync environment

# Clean project
rm -rf .venv               # Remove virtual environment
rm -rf dist build *.egg-info  # Remove build artifacts
rm -rf .pytest_cache .mypy_cache __pycache__  # Remove caches

# Rebuild environment
rm -rf .venv && uv sync
```

## Best Practices

1. **Always use pyproject.toml** - Single source of configuration
2. **Pin Python version** - Use `.python-version` file
3. **Lock dependencies** - Commit `uv.lock` to git
4. **Separate dev deps** - Use `--dev` for development tools
5. **Configure tools** - Add ruff, mypy, pytest config to pyproject.toml
6. **Use src layout** - Place code in `src/package/` for better testing
7. **Add type hints** - Enable mypy strict mode
8. **Write tests** - Aim for >80% coverage
9. **Use entry points** - Define CLI commands in `[project.scripts]`
10. **Document** - Add docstrings and README
