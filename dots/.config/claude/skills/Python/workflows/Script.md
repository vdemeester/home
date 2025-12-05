# Script Workflow

Create and run single-file Python scripts with inline dependencies using PEP 723 and uv.

## When to Use

- "create python script"
- "single file script"
- "uv run"
- "PEP 723"
- "standalone script"

## Quick Start

### Basic Script with Dependencies

```python
#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "requests>=2.31.0",
#     "rich>=13.0.0",
# ]
# ///
"""Fetch and display data from an API."""

import requests
from rich.console import Console
from rich.table import Table

def main() -> None:
    """Fetch data and display in a table."""
    console = Console()

    with console.status("[bold green]Fetching data..."):
        response = requests.get("https://api.github.com/users/github")
        response.raise_for_status()
        data = response.json()

    table = Table(title="GitHub User Info")
    table.add_column("Field", style="cyan")
    table.add_column("Value", style="magenta")

    table.add_row("Name", data.get("name", "N/A"))
    table.add_row("Location", data.get("location", "N/A"))
    table.add_row("Public Repos", str(data.get("public_repos", 0)))

    console.print(table)

if __name__ == "__main__":
    main()
```

### Run the Script

```bash
# Make executable
chmod +x script.py

# Run directly (uv handles dependencies automatically)
./script.py

# Or run with uv explicitly
uv run script.py
```

## PEP 723 Inline Metadata

### Metadata Block Format

The metadata must be:
- In a comment block starting with `# /// script`
- Followed by TOML-formatted metadata
- Closed with `# ///`
- Placed before any imports

```python
#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "requests>=2.31.0",
#     "rich>=13.0.0",
# ]
# ///
```

### Shebang Line

**Required format:**
```python
#!/usr/bin/env -S uv run --script
```

**Important:** Must use `--script` flag with `uv run` in shebang.

### Dependencies Syntax

```python
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     # Simple dependency
#     "requests",
#
#     # With version constraint
#     "rich>=13.0.0",
#
#     # Version range
#     "httpx>=0.24.0,<1.0.0",
#
#     # Git dependency
#     "mypackage @ git+https://github.com/user/repo.git",
#
#     # Git with tag/branch
#     "mypackage @ git+https://github.com/user/repo.git@v1.0.0",
# ]
# ///
```

## Script Templates

### Data Processing Script

```python
#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "pandas>=2.0.0",
#     "rich>=13.0.0",
# ]
# ///
"""Process CSV data and generate report."""

import sys
from pathlib import Path
from typing import NoReturn

import pandas as pd
from rich.console import Console
from rich.table import Table

console = Console()

def process_csv(input_path: Path, output_path: Path) -> None:
    """Process CSV file and generate summary.

    Args:
        input_path: Path to input CSV file
        output_path: Path to output summary file

    Raises:
        FileNotFoundError: If input file doesn't exist
        ValueError: If CSV is invalid or empty
    """
    if not input_path.exists():
        raise FileNotFoundError(f"Input file not found: {input_path}")

    # Read and process
    df = pd.read_csv(input_path)

    if df.empty:
        raise ValueError("CSV file is empty")

    # Generate summary
    summary = df.describe()

    # Save output
    summary.to_csv(output_path)

    # Display
    table = Table(title=f"Summary: {input_path.name}")
    for col in summary.columns:
        table.add_column(col)

    for idx, row in summary.iterrows():
        table.add_row(str(idx), *[f"{val:.2f}" for val in row])

    console.print(table)
    console.print(f"\n[green]Summary saved to:[/green] {output_path}")

def main() -> int:
    """Run the data processing script.

    Returns:
        Exit code (0 for success, 1 for error)
    """
    if len(sys.argv) != 3:
        console.print("[red]Usage:[/red] script.py <input.csv> <output.csv>")
        return 1

    input_path = Path(sys.argv[1])
    output_path = Path(sys.argv[2])

    try:
        process_csv(input_path, output_path)
        return 0
    except Exception as e:
        console.print(f"[red]Error:[/red] {e}")
        return 1

if __name__ == "__main__":
    sys.exit(main())
```

### API Client Script

```python
#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "httpx>=0.24.0",
#     "pydantic>=2.0.0",
#     "rich>=13.0.0",
# ]
# ///
"""Fetch and validate data from REST API."""

import sys
from typing import Any

import httpx
from pydantic import BaseModel, Field, ValidationError
from rich.console import Console
from rich.pretty import pprint

console = Console()

class User(BaseModel):
    """User data model."""

    id: int
    name: str
    email: str
    company: dict[str, Any] = Field(default_factory=dict)

    class Config:
        """Pydantic configuration."""

        frozen = True

async def fetch_user(user_id: int) -> User:
    """Fetch user from API.

    Args:
        user_id: ID of user to fetch

    Returns:
        User object with validated data

    Raises:
        httpx.HTTPError: If request fails
        ValidationError: If response data is invalid
    """
    async with httpx.AsyncClient() as client:
        response = await client.get(
            f"https://jsonplaceholder.typicode.com/users/{user_id}"
        )
        response.raise_for_status()
        data = response.json()

    return User(**data)

async def main() -> int:
    """Run the API client.

    Returns:
        Exit code (0 for success, 1 for error)
    """
    if len(sys.argv) != 2:
        console.print("[red]Usage:[/red] script.py <user_id>")
        return 1

    try:
        user_id = int(sys.argv[1])
    except ValueError:
        console.print("[red]Error:[/red] user_id must be an integer")
        return 1

    try:
        with console.status(f"[bold green]Fetching user {user_id}..."):
            user = await fetch_user(user_id)

        console.print("\n[bold green]User data:[/bold green]")
        pprint(user.model_dump())
        return 0

    except httpx.HTTPError as e:
        console.print(f"[red]HTTP Error:[/red] {e}")
        return 1
    except ValidationError as e:
        console.print(f"[red]Validation Error:[/red] {e}")
        return 1
    except Exception as e:
        console.print(f"[red]Error:[/red] {e}")
        return 1

if __name__ == "__main__":
    import asyncio
    sys.exit(asyncio.run(main()))
```

### CLI Tool Script

```python
#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "click>=8.0.0",
#     "rich>=13.0.0",
# ]
# ///
"""Example CLI tool with multiple commands."""

import click
from rich.console import Console

console = Console()

@click.group()
@click.version_option(version="1.0.0")
def cli() -> None:
    """My awesome CLI tool."""

@cli.command()
@click.argument("name")
@click.option("--greeting", default="Hello", help="Greeting to use")
def greet(name: str, greeting: str) -> None:
    """Greet someone by name.

    Args:
        name: Name to greet
        greeting: Greeting message
    """
    console.print(f"[bold green]{greeting}, {name}![/bold green]")

@cli.command()
@click.argument("numbers", nargs=-1, type=int)
@click.option("--operation", type=click.Choice(["sum", "product"]), default="sum")
def calculate(numbers: tuple[int, ...], operation: str) -> None:
    """Perform calculation on numbers.

    Args:
        numbers: Numbers to calculate
        operation: Operation to perform (sum or product)
    """
    if not numbers:
        console.print("[red]Error:[/red] No numbers provided")
        return

    if operation == "sum":
        result = sum(numbers)
        console.print(f"Sum: [bold]{result}[/bold]")
    else:
        result = 1
        for num in numbers:
            result *= num
        console.print(f"Product: [bold]{result}[/bold]")

if __name__ == "__main__":
    cli()
```

### File Processing Script

```python
#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "pathlib",
# ]
# ///
"""Process files in a directory."""

import sys
from pathlib import Path
from typing import Iterator

def find_python_files(directory: Path) -> Iterator[Path]:
    """Find all Python files in directory.

    Args:
        directory: Directory to search

    Yields:
        Path objects for Python files
    """
    for path in directory.rglob("*.py"):
        if path.is_file() and not path.name.startswith("."):
            yield path

def count_lines(file_path: Path) -> int:
    """Count lines in file.

    Args:
        file_path: Path to file

    Returns:
        Number of lines in file
    """
    return len(file_path.read_text().splitlines())

def main() -> int:
    """Process Python files and count lines.

    Returns:
        Exit code (0 for success, 1 for error)
    """
    if len(sys.argv) != 2:
        print("Usage: script.py <directory>")
        return 1

    directory = Path(sys.argv[1])

    if not directory.is_dir():
        print(f"Error: {directory} is not a directory")
        return 1

    total_lines = 0
    file_count = 0

    for file_path in find_python_files(directory):
        lines = count_lines(file_path)
        total_lines += lines
        file_count += 1
        print(f"{file_path.relative_to(directory)}: {lines} lines")

    print(f"\nTotal: {file_count} files, {total_lines} lines")
    return 0

if __name__ == "__main__":
    sys.exit(main())
```

## Managing Script Dependencies

### Add Dependencies to Existing Script

```bash
# Add dependency to script
uv add --script myscript.py requests

# Add multiple dependencies
uv add --script myscript.py requests rich httpx
```

This updates the inline metadata in the script automatically.

### Remove Dependencies

Manually edit the script's metadata block to remove dependencies.

## Testing Scripts

### Inline Tests

```python
#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "pytest>=8.0.0",
# ]
# ///
"""Script with inline tests."""

def add(a: int, b: int) -> int:
    """Add two numbers."""
    return a + b

def test_add() -> None:
    """Test add function."""
    assert add(2, 3) == 5
    assert add(0, 0) == 0
    assert add(-1, 1) == 0

if __name__ == "__main__":
    # Run main logic
    result = add(10, 20)
    print(f"Result: {result}")

    # To run tests: pytest script.py
```

```bash
# Run the script normally
./script.py

# Run tests
uv run pytest script.py
```

## Script Organization

### Multiple Related Scripts

For related scripts, consider this structure:

```
scripts/
├── common.py              # Shared utilities (no shebang)
├── fetch_data.py          # Script 1 (with shebang + metadata)
├── process_data.py        # Script 2 (with shebang + metadata)
└── generate_report.py     # Script 3 (with shebang + metadata)
```

**Shared utilities (common.py):**
```python
"""Shared utilities for scripts."""
from pathlib import Path

def ensure_dir(path: Path) -> None:
    """Ensure directory exists."""
    path.mkdir(parents=True, exist_ok=True)
```

**Script using shared code:**
```python
#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///
"""Script using shared utilities."""

import sys
from pathlib import Path

# Import from adjacent file
sys.path.insert(0, str(Path(__file__).parent))
from common import ensure_dir

def main() -> int:
    ensure_dir(Path("output"))
    return 0

if __name__ == "__main__":
    sys.exit(main())
```

## Best Practices

1. **Always use shebang** - `#!/usr/bin/env -S uv run --script`
2. **Always use metadata block** - Even if no dependencies
3. **Pin Python version** - Use `requires-python`
4. **Use version constraints** - Pin dependency versions
5. **Add docstrings** - Document what the script does
6. **Add type hints** - Make code more maintainable
7. **Handle errors** - Use try/except and return error codes
8. **Make executable** - `chmod +x script.py`
9. **Use pathlib** - Use `Path` objects instead of strings
10. **Return exit codes** - Return 0 for success, non-zero for errors

## Common Patterns

### Read from stdin, write to stdout

```python
#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///
"""Process data from stdin to stdout."""

import sys

def main() -> int:
    """Read from stdin, process, write to stdout."""
    for line in sys.stdin:
        processed = line.strip().upper()
        print(processed)
    return 0

if __name__ == "__main__":
    sys.exit(main())
```

```bash
# Usage
echo "hello world" | ./script.py
cat input.txt | ./script.py > output.txt
```

### Configuration from environment

```python
#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///
"""Script using environment variables."""

import os
import sys

def main() -> int:
    """Run script with config from environment."""
    api_key = os.getenv("API_KEY")
    if not api_key:
        print("Error: API_KEY environment variable not set", file=sys.stderr)
        return 1

    # Use api_key...
    print(f"Using API key: {api_key[:4]}...")
    return 0

if __name__ == "__main__":
    sys.exit(main())
```

```bash
# Usage
API_KEY=secret ./script.py
```
