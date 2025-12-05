# Type Checking Workflow

Type check Python code with mypy for static type verification.

## When to Use

- "type check python"
- "mypy"
- "check type hints"
- "static type checking"

## Quick Commands

```bash
# Type check all files
uv run mypy .

# Type check specific file/directory
uv run mypy src/myproject

# Strict mode
uv run mypy --strict .

# Show error codes
uv run mypy --show-error-codes .

# Generate HTML report
uv run mypy --html-report mypy-report .
```

## Configuration

### pyproject.toml

```toml
[tool.mypy]
python_version = "3.11"
strict = true

# Import discovery
files = ["src", "tests"]
namespace_packages = true
explicit_package_bases = true

# Warnings
warn_return_any = true
warn_unused_configs = true
warn_redundant_casts = true
warn_unused_ignores = true
warn_no_return = true
warn_unreachable = true

# Strictness
disallow_untyped_defs = true
disallow_any_generics = true
disallow_subclassing_any = true
disallow_untyped_calls = true
disallow_incomplete_defs = true
check_untyped_defs = true
disallow_untyped_decorators = true
no_implicit_optional = true
strict_optional = true
strict_equality = true

# Error messages
show_error_context = true
show_column_numbers = true
show_error_codes = true
pretty = true

# Per-module options
[[tool.mypy.overrides]]
module = "tests.*"
disallow_untyped_defs = false

[[tool.mypy.overrides]]
module = "setuptools.*"
ignore_missing_imports = true
```

## Type Hints Examples

### Basic Types

```python
from typing import Any

# Simple types
name: str = "John"
age: int = 30
height: float = 5.9
is_active: bool = True

# Collections
names: list[str] = ["John", "Jane"]
scores: dict[str, int] = {"math": 90, "english": 85}
coordinates: tuple[float, float] = (1.0, 2.0)
unique_ids: set[int] = {1, 2, 3}

# Optional
optional_value: str | None = None
```

### Function Annotations

```python
def greet(name: str, age: int = 0) -> str:
    """Greet a person."""
    return f"Hello {name}, age {age}"

def process_items(items: list[int]) -> dict[str, int]:
    """Process list of items."""
    return {"count": len(items), "sum": sum(items)}

# No return value
def log_message(msg: str) -> None:
    """Log a message."""
    print(msg)
```

### Type Aliases

```python
from typing import TypeAlias

# Simple alias
UserId: TypeAlias = int
Username: TypeAlias = str

# Complex alias
UserData: TypeAlias = dict[str, str | int]
ProcessingResult: TypeAlias = tuple[bool, str, list[int]]

def get_user(user_id: UserId) -> UserData:
    """Get user data."""
    return {"id": user_id, "name": "John"}
```

### Generics

```python
from typing import TypeVar, Generic

T = TypeVar("T")

class Box(Generic[T]):
    """Generic box container."""

    def __init__(self, value: T) -> None:
        self.value = value

    def get(self) -> T:
        return self.value

# Usage
int_box: Box[int] = Box(42)
str_box: Box[str] = Box("hello")
```

### Protocols

```python
from typing import Protocol

class Drawable(Protocol):
    """Protocol for drawable objects."""

    def draw(self) -> str:
        """Draw the object."""
        ...

class Circle:
    """Circle that implements Drawable protocol."""

    def draw(self) -> str:
        return "Drawing circle"

def render(obj: Drawable) -> None:
    """Render any drawable object."""
    print(obj.draw())
```

### Callable Types

```python
from typing import Callable

# Function that takes int, returns str
Transformer: TypeAlias = Callable[[int], str]

def apply_transform(
    value: int,
    transform: Transformer,
) -> str:
    """Apply transformation function."""
    return transform(value)

# Usage
def int_to_str(x: int) -> str:
    return str(x)

result = apply_transform(42, int_to_str)
```

## Advanced Patterns

### Literal Types

```python
from typing import Literal

def set_mode(mode: Literal["read", "write", "append"]) -> None:
    """Set file mode."""
    pass

# Only accepts these exact strings
set_mode("read")   # OK
set_mode("delete") # Error!
```

### TypedDict

```python
from typing import TypedDict

class User(TypedDict):
    """User data structure."""
    id: int
    name: str
    email: str
    age: int | None  # Optional field

def create_user(user: User) -> None:
    """Create a user."""
    print(user["name"])

# Usage
user: User = {
    "id": 1,
    "name": "John",
    "email": "john@example.com",
    "age": None,
}
```

### Union Types

```python
def process(value: int | str | None) -> str:
    """Process value of multiple types."""
    if value is None:
        return "empty"
    if isinstance(value, int):
        return f"number: {value}"
    return f"string: {value}"
```

## Type Checking Best Practices

1. **Start with `--strict`** - Catches most issues
2. **Add type hints everywhere** - Functions, variables, attributes
3. **Use type aliases** - For complex types
4. **Leverage protocols** - For structural subtyping
5. **Check CI** - Run mypy in continuous integration
6. **Fix incrementally** - Use per-file overrides if needed
7. **Type stubs** - Install type stubs for third-party libraries

```bash
# Install type stubs
uv add --dev types-requests types-pyyaml
```

## Common Issues

### Ignoring Errors

```python
# Ignore error on specific line
result = unsafe_function()  # type: ignore

# Ignore specific error code
result = unsafe_function()  # type: ignore[arg-type]

# File-level ignore (use sparingly)
# mypy: ignore-errors
```

### Dealing with `Any`

```python
from typing import Any, cast

# Avoid Any when possible
def bad(data: Any) -> Any:
    return data

# Better: Be specific
def good(data: dict[str, str]) -> dict[str, str]:
    return data

# If Any is unavoidable, cast to specific type
raw_data: Any = get_external_data()
user_data: dict[str, str] = cast(dict[str, str], raw_data)
```
