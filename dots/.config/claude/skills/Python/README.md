# Python Skill

Modern Python development guidance using uv, ruff, mypy, and pytest.

## Overview

This skill provides comprehensive guidance for Python development following 2025 best practices with modern tooling:

- **uv**: Fast, all-in-one package manager (replaces pip, poetry, pyenv, virtualenv)
- **ruff**: Lightning-fast linter and formatter (replaces Black, isort, flake8, pylint)
- **mypy**: Static type checker for type safety
- **pytest**: Feature-rich testing framework

## Workflows

### Project Management
- **[Project](workflows/Project.md)**: Create and configure Python projects
- **[Script](workflows/Script.md)**: Single-file scripts with PEP 723 inline metadata
- **[Workspace](workflows/Workspace.md)**: Manage monorepos with multiple packages

### Code Quality
- **[Lint](workflows/Lint.md)**: Lint and format code with ruff
- **[Type](workflows/Type.md)**: Type check with mypy
- **[Test](workflows/Test.md)**: Run tests with pytest and coverage

### Dependencies
- **[Deps](workflows/Deps.md)**: Manage dependencies with uv
- **[Package](workflows/Package.md)**: Build and publish packages

## Tools

### python-lint
Run ruff linter and formatter on your code:
```bash
./tools/python-lint [directory]
```

### python-check
Run all quality checks (lint, type check, tests):
```bash
./tools/python-check [directory]
```

## Quick Start

### New Project
```bash
# Create project
uv init myproject
cd myproject

# Add dependencies
uv add requests pydantic rich
uv add --dev pytest ruff mypy

# Run checks
uv run ruff check --fix .
uv run ruff format .
uv run mypy .
uv run pytest
```

### Single Script
```python
#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "requests>=2.31.0",
# ]
# ///
import requests

def main() -> int:
    response = requests.get("https://api.github.com")
    print(response.json())
    return 0

if __name__ == "__main__":
    import sys
    sys.exit(main())
```

```bash
chmod +x script.py
./script.py  # uv handles dependencies automatically!
```

## Key Features

### PEP 723 Support
Inline script metadata allows standalone scripts to declare dependencies directly in the file. No separate requirements.txt needed!

### Modern Type Hints
Full support for Python 3.10+ type syntax:
```python
def process(items: list[str], config: dict[str, int] | None = None) -> list[int]:
    """Process items with optional config."""
    pass
```

### Fast Tooling
- uv is 10-100x faster than pip
- ruff is 10-100x faster than traditional linters
- Both written in Rust for maximum performance

## Best Practices

1. **Use uv for everything** - Package management, venv, running scripts
2. **Type hints everywhere** - Enable mypy strict mode
3. **Lint with ruff** - Auto-fix before committing
4. **Test with pytest** - Aim for >80% coverage
5. **Lock dependencies** - Commit uv.lock to git
6. **Use PEP 723** - For single-file scripts
7. **Follow src layout** - Place code in `src/package/`

## Resources

### Documentation
- [uv Documentation](https://docs.astral.sh/uv/)
- [PEP 723 - Inline Script Metadata](https://peps.python.org/pep-0723/)
- [Ruff Documentation](https://docs.astral.sh/ruff/)
- [mypy Documentation](https://mypy.readthedocs.io/)
- [pytest Documentation](https://docs.pytest.org/)

### Articles
- [Python UV: The Ultimate Guide (DataCamp)](https://www.datacamp.com/tutorial/python-uv)
- [Managing Python Projects With uv (Real Python)](https://realpython.com/python-uv/)
- [Share Python Scripts Like a Pro: uv and PEP 723](https://thisdavej.com/share-python-scripts-like-a-pro-uv-and-pep-723-for-easy-deployment/)
- [Python Testing Best Practices](https://pytest-with-eric.com/introduction/python-unit-testing-best-practices/)
