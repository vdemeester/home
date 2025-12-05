---
name: Python
description: Python development best practices and modern tooling with uv. USE WHEN writing Python code, managing dependencies, testing, type checking, or working with Python projects.
---

# Python Development Best Practices

## Purpose
Guide Python development using modern tooling (uv, ruff, mypy, pytest) following Python Enhancement Proposals (PEPs), type hints best practices, and community standards for 2025.

### Context Detection

**This skill activates when:**
- Current directory contains `pyproject.toml`, `requirements.txt`, or `uv.lock`
- Git repository contains Python files (`.py`, `.pyi`)
- User is working with Python code or virtual environments
- Commands like `uv`, `pytest`, `ruff`, or `mypy` are mentioned
- User explicitly asks about Python development

## Workflow Routing

**When executing a workflow, output this notification directly:**

```
Running the **WorkflowName** workflow from the **Python** skill...
```

| Workflow | Trigger | File |
|----------|---------|------|
| **Project** | "create python project", "new project", "uv init" | `workflows/Project.md` |
| **Script** | "python script", "single file", "uv run", "PEP 723" | `workflows/Script.md` |
| **Test** | "run tests", "pytest", "test coverage", "tox" | `workflows/Test.md` |
| **Lint** | "lint", "format", "ruff check", "ruff format" | `workflows/Lint.md` |
| **Type** | "type check", "mypy", "type hints", "pyright" | `workflows/Type.md` |
| **Deps** | "dependencies", "uv add", "requirements", "lock file" | `workflows/Deps.md` |
| **Package** | "build package", "publish", "pypi", "wheel" | `workflows/Package.md` |
| **Workspace** | "monorepo", "workspace", "multiple packages" | `workflows/Workspace.md` |

## Core Principles

1. **Use uv for everything**: uv is the modern, fast, all-in-one tool replacing pip, pip-tools, poetry, pyenv, virtualenv, and more (10-100x faster)
2. **Type hints everywhere**: Use type annotations for all functions and classes; run mypy in strict mode
3. **Ruff for quality**: Use ruff for both linting and formatting (replaces Black, isort, flake8, and more)
4. **Test with pytest**: Write comprehensive tests using pytest with fixtures and parametrization
5. **Lock dependencies**: Always maintain `uv.lock` for reproducible builds
6. **PEP 723 for scripts**: Use inline script metadata for single-file Python scripts

## Modern Python Toolchain (2025)

### Package Management: uv

**Why uv?**
- Written in Rust, 10-100x faster than pip
- All-in-one: replaces pip, virtualenv, poetry, pyenv, pipx
- Lockfile support for reproducible builds
- Automatic Python version management
- First-class support for PEP 723 inline script metadata

**Installation:**
```bash
# On NixOS (preferred in this repo)
# Already available via home-manager or system packages

# Standalone (if needed elsewhere)
curl -LsSf https://astral.sh/uv/install.sh | sh
```

### Code Quality: ruff

**Why ruff?**
- Extremely fast (10-100x faster than traditional tools)
- Replaces: Black, isort, flake8, pylint, pyupgrade, autoflake
- Single tool for linting and formatting
- Compatible with Black formatting

**Usage:**
```bash
# Check for issues
ruff check .

# Auto-fix issues
ruff check --fix .

# Format code
ruff format .
```

### Type Checking: mypy

**Why mypy?**
- Industry standard for static type checking
- Catches bugs before runtime
- Improves code documentation and IDE support
- Works alongside ruff (not replaced by it)

**Usage:**
```bash
# Type check entire project
mypy .

# Strict mode
mypy --strict .

# With specific Python version
mypy --python-executable .venv/bin/python .
```

### Testing: pytest

**Why pytest?**
- Most popular testing framework
- Simple syntax, powerful features
- Excellent fixture system
- Parametrization for data-driven tests
- Rich plugin ecosystem

**Usage:**
```bash
# Run all tests
pytest

# With coverage
pytest --cov

# Specific test
pytest tests/test_auth.py::test_login
```

## Project Structure

### Standard Layout
```
myproject/
├── src/
│   └── myproject/
│       ├── __init__.py
│       ├── main.py
│       └── utils.py
├── tests/
│   ├── __init__.py
│   ├── test_main.py
│   └── test_utils.py
├── docs/
│   └── README.md
├── .gitignore
├── .python-version          # Python version (managed by uv)
├── pyproject.toml          # Project configuration
├── uv.lock                 # Locked dependencies
└── README.md
```

### Key Files

**`pyproject.toml`** - Project configuration (PEP 621)
```toml
[project]
name = "myproject"
version = "0.1.0"
description = "Description of my project"
readme = "README.md"
requires-python = ">=3.11"
dependencies = [
    "requests>=2.31.0",
]

[project.optional-dependencies]
dev = [
    "pytest>=8.0.0",
    "ruff>=0.8.0",
    "mypy>=1.13.0",
]

[build-system]
requires = ["hatchling"]
build-backend = "hatchling.build"

[tool.ruff]
line-length = 100
target-version = "py311"

[tool.ruff.lint]
select = ["E", "F", "I", "N", "W", "B", "Q"]

[tool.mypy]
python_version = "3.11"
strict = true
warn_return_any = true
warn_unused_configs = true

[tool.pytest.ini_options]
testpaths = ["tests"]
python_files = "test_*.py"
python_functions = "test_*"
```

**`.python-version`** - Pin Python version
```
3.11
```

## Writing Python Code

### Type Hints

**Always use type hints:**
```python
from typing import List, Dict, Optional, Union
from pathlib import Path

def process_data(
    items: list[str],
    config: dict[str, int],
    output_path: Path | None = None,
) -> list[int]:
    """Process items according to configuration.

    Args:
        items: List of items to process
        config: Configuration dictionary
        output_path: Optional path to write results

    Returns:
        List of processed integers

    Raises:
        ValueError: If items is empty
    """
    if not items:
        raise ValueError("items cannot be empty")

    results: list[int] = []
    for item in items:
        value = config.get(item, 0)
        results.append(value)

    if output_path is not None:
        output_path.write_text(str(results))

    return results
```

**Modern type syntax (Python 3.10+):**
```python
# Use | instead of Union
def foo(x: int | str) -> str: ...

# Use list, dict, tuple directly (no need for typing.List, etc.)
def bar(items: list[int]) -> dict[str, list[int]]: ...
```

### Error Handling

**Be explicit about errors:**
```python
from pathlib import Path
import logging

logger = logging.getLogger(__name__)

def read_config(path: Path) -> dict[str, str]:
    """Read configuration from file.

    Args:
        path: Path to configuration file

    Returns:
        Configuration dictionary

    Raises:
        FileNotFoundError: If config file doesn't exist
        ValueError: If config file is invalid
    """
    if not path.exists():
        raise FileNotFoundError(f"Config file not found: {path}")

    try:
        content = path.read_text()
        # Parse content...
        config = parse_config(content)
    except Exception as e:
        logger.error(f"Failed to parse config: {e}")
        raise ValueError(f"Invalid config file: {path}") from e

    return config
```

### Docstrings

**Use Google or NumPy style docstrings:**
```python
def calculate_metrics(
    data: list[float],
    method: str = "mean",
) -> dict[str, float]:
    """Calculate statistical metrics for data.

    Args:
        data: List of numeric values
        method: Calculation method ("mean", "median", "mode")

    Returns:
        Dictionary containing calculated metrics with keys:
        - value: The calculated metric
        - confidence: Confidence interval

    Raises:
        ValueError: If method is not supported or data is empty

    Examples:
        >>> calculate_metrics([1, 2, 3], method="mean")
        {'value': 2.0, 'confidence': 0.95}
    """
    if not data:
        raise ValueError("data cannot be empty")

    # Implementation...
```

## Single-File Scripts (PEP 723)

**Use uv with inline script metadata for standalone scripts:**

```python
#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "requests>=2.31.0",
#     "rich>=13.0.0",
# ]
# ///
"""Script to fetch and display API data."""

import requests
from rich.console import Console

console = Console()

def main() -> None:
    """Fetch and display data from API."""
    response = requests.get("https://api.example.com/data")
    response.raise_for_status()

    data = response.json()
    console.print(data)

if __name__ == "__main__":
    main()
```

**Make it executable:**
```bash
chmod +x script.py
./script.py  # uv automatically handles dependencies!
```

## Testing with pytest

### Test Structure

```python
# tests/test_calculator.py
import pytest
from myproject.calculator import Calculator

class TestCalculator:
    """Test suite for Calculator class."""

    @pytest.fixture
    def calc(self) -> Calculator:
        """Provide a Calculator instance."""
        return Calculator()

    def test_add(self, calc: Calculator) -> None:
        """Test addition operation."""
        result = calc.add(2, 3)
        assert result == 5

    @pytest.mark.parametrize("a,b,expected", [
        (2, 3, 5),
        (0, 0, 0),
        (-1, 1, 0),
        (10, -5, 5),
    ])
    def test_add_parametrized(
        self,
        calc: Calculator,
        a: int,
        b: int,
        expected: int,
    ) -> None:
        """Test addition with multiple inputs."""
        assert calc.add(a, b) == expected

    def test_divide_by_zero(self, calc: Calculator) -> None:
        """Test division by zero raises error."""
        with pytest.raises(ValueError, match="Cannot divide by zero"):
            calc.divide(10, 0)
```

### Coverage Configuration

Add to `pyproject.toml`:
```toml
[tool.coverage.run]
source = ["src"]
omit = ["tests/*", "**/__pycache__/*"]

[tool.coverage.report]
precision = 2
show_missing = true
skip_covered = false

[tool.coverage.html]
directory = "htmlcov"
```

## Common Workflows

### New Project
```bash
# Create new project
uv init myproject
cd myproject

# Add dependencies
uv add requests rich

# Add dev dependencies
uv add --dev pytest ruff mypy

# Run tests
uv run pytest
```

### Existing Project
```bash
# Sync environment with lock file
uv sync

# Run app
uv run python -m myproject

# Run tests
uv run pytest

# Lint and format
uv run ruff check --fix .
uv run ruff format .

# Type check
uv run mypy .
```

### Scripts
```bash
# Run script (with automatic dependency installation)
uv run script.py

# Add dependencies to existing script
uv add --script script.py requests
```

## Integration with NixOS

This repository uses NixOS with home-manager. Python tools are configured via:

**System/Home packages:**
```nix
# home/common/dev/python.nix
{ pkgs, ... }: {
  home.packages = with pkgs; [
    uv          # Package manager
    ruff        # Linter/formatter
    mypy        # Type checker
    python311   # Python interpreter
  ];
}
```

**Shell environment:**
```bash
# uv manages virtual environments automatically
# No need for manual venv activation

# Tools available system-wide
uv --version
ruff --version
mypy --version
```

## Best Practices Checklist

- [ ] Use `uv` for all package management
- [ ] Add type hints to all functions
- [ ] Run `mypy --strict` with no errors
- [ ] Use `ruff` for linting and formatting
- [ ] Write tests with `pytest` (aim for >80% coverage)
- [ ] Use PEP 723 for single-file scripts
- [ ] Pin Python version in `.python-version`
- [ ] Lock dependencies in `uv.lock`
- [ ] Document functions with docstrings
- [ ] Handle errors explicitly
- [ ] Use `pathlib.Path` instead of string paths
- [ ] Follow PEP 8 naming conventions
- [ ] Keep functions focused and small
- [ ] Use descriptive variable names

## Resources

### Official Documentation
- [uv Documentation](https://docs.astral.sh/uv/)
- [PEP 723 - Inline Script Metadata](https://peps.python.org/pep-0723/)
- [Ruff Documentation](https://docs.astral.sh/ruff/)
- [mypy Documentation](https://mypy.readthedocs.io/)
- [pytest Documentation](https://docs.pytest.org/)

### Best Practices
- [Python UV: The Ultimate Guide (DataCamp)](https://www.datacamp.com/tutorial/python-uv)
- [uv: An In-Depth Guide](https://www.saaspegasus.com/guides/uv-deep-dive/)
- [Managing Python Projects With uv (Real Python)](https://realpython.com/python-uv/)
- [Python Testing Best Practices](https://pytest-with-eric.com/introduction/python-unit-testing-best-practices/)

### References
- Python Enhancement Proposals (PEPs)
- Google Python Style Guide
- Type Hints Best Practices
- pytest Good Integration Practices
