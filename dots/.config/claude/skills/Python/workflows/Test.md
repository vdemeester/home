# Test Workflow

Run Python tests with pytest, coverage, and various testing patterns.

## When to Use

- "run python tests"
- "pytest"
- "test coverage"
- "run specific test"
- "test with fixtures"

## Quick Commands

### Basic Testing

```bash
# Run all tests
uv run pytest

# Verbose output
uv run pytest -v

# Very verbose (show test names as they run)
uv run pytest -vv

# Run specific test file
uv run pytest tests/test_auth.py

# Run specific test function
uv run pytest tests/test_auth.py::test_login

# Run specific test class
uv run pytest tests/test_auth.py::TestAuth

# Run specific test method
uv run pytest tests/test_auth.py::TestAuth::test_login

# Run tests matching pattern
uv run pytest -k "auth"
uv run pytest -k "test_login or test_logout"
```

### Test Coverage

```bash
# Run tests with coverage
uv run pytest --cov

# Coverage for specific module
uv run pytest --cov=src/myproject

# Show missing lines
uv run pytest --cov --cov-report=term-missing

# Generate HTML coverage report
uv run pytest --cov --cov-report=html
# Open htmlcov/index.html in browser

# Generate XML coverage (for CI)
uv run pytest --cov --cov-report=xml

# Fail if coverage below threshold
uv run pytest --cov --cov-fail-under=80
```

### Test Output Control

```bash
# Show print statements
uv run pytest -s

# Stop at first failure
uv run pytest -x

# Stop after N failures
uv run pytest --maxfail=3

# Show local variables in traceback
uv run pytest -l

# Show summary of all test outcomes
uv run pytest -ra
```

### Test Selection

```bash
# Run only failed tests from last run
uv run pytest --lf

# Run failed tests first, then others
uv run pytest --ff

# Run tests in random order (requires pytest-random-order)
uv run pytest --random-order
```

## Test Structure

### Basic Test File

```python
# tests/test_calculator.py
"""Tests for calculator module."""
import pytest
from myproject.calculator import Calculator

class TestCalculator:
    """Test suite for Calculator class."""

    def test_add(self) -> None:
        """Test addition operation."""
        calc = Calculator()
        result = calc.add(2, 3)
        assert result == 5

    def test_subtract(self) -> None:
        """Test subtraction operation."""
        calc = Calculator()
        result = calc.subtract(5, 3)
        assert result == 2

    def test_multiply(self) -> None:
        """Test multiplication operation."""
        calc = Calculator()
        result = calc.multiply(2, 3)
        assert result == 6

    def test_divide(self) -> None:
        """Test division operation."""
        calc = Calculator()
        result = calc.divide(6, 3)
        assert result == 2.0

    def test_divide_by_zero(self) -> None:
        """Test division by zero raises error."""
        calc = Calculator()
        with pytest.raises(ValueError, match="Cannot divide by zero"):
            calc.divide(10, 0)
```

### Using Fixtures

```python
# tests/test_database.py
"""Tests for database operations."""
import pytest
from myproject.database import Database

@pytest.fixture
def db() -> Database:
    """Provide a test database instance."""
    database = Database(":memory:")  # SQLite in-memory
    database.create_tables()
    yield database
    database.close()

@pytest.fixture
def sample_data() -> dict[str, str]:
    """Provide sample test data."""
    return {
        "name": "John Doe",
        "email": "john@example.com",
    }

def test_insert_user(db: Database, sample_data: dict[str, str]) -> None:
    """Test inserting a user into database."""
    user_id = db.insert_user(sample_data)
    assert user_id > 0

def test_get_user(db: Database, sample_data: dict[str, str]) -> None:
    """Test retrieving a user from database."""
    user_id = db.insert_user(sample_data)
    user = db.get_user(user_id)
    assert user["name"] == sample_data["name"]
    assert user["email"] == sample_data["email"]
```

### Parametrized Tests

```python
# tests/test_validation.py
"""Tests for validation functions."""
import pytest
from myproject.validation import validate_email, validate_phone

@pytest.mark.parametrize("email,expected", [
    ("user@example.com", True),
    ("user.name@example.co.uk", True),
    ("user+tag@example.com", True),
    ("invalid.email", False),
    ("@example.com", False),
    ("user@", False),
    ("", False),
])
def test_validate_email(email: str, expected: bool) -> None:
    """Test email validation with various inputs."""
    assert validate_email(email) == expected

@pytest.mark.parametrize("phone,expected", [
    ("+1-555-123-4567", True),
    ("555-123-4567", True),
    ("5551234567", True),
    ("invalid", False),
    ("", False),
])
def test_validate_phone(phone: str, expected: bool) -> None:
    """Test phone validation with various inputs."""
    assert validate_phone(phone) == expected

# Test with multiple parameters
@pytest.mark.parametrize("a,b,expected", [
    (2, 3, 5),
    (0, 0, 0),
    (-1, 1, 0),
    (10, -5, 5),
    (100, 200, 300),
])
def test_addition(a: int, b: int, expected: int) -> None:
    """Test addition with multiple input combinations."""
    assert a + b == expected
```

### Fixture Scopes

```python
# tests/conftest.py
"""Shared fixtures for all tests."""
import pytest
from myproject.app import create_app
from myproject.database import Database

@pytest.fixture(scope="session")
def app():
    """Provide application instance for entire test session."""
    app = create_app({"TESTING": True})
    yield app

@pytest.fixture(scope="module")
def db():
    """Provide database for all tests in a module."""
    database = Database(":memory:")
    database.create_tables()
    yield database
    database.close()

@pytest.fixture(scope="function")
def clean_db(db: Database):
    """Provide clean database for each test function."""
    yield db
    db.clear_all_tables()  # Clean up after each test

@pytest.fixture
def client(app):
    """Provide test client for making requests."""
    return app.test_client()
```

## Advanced Testing Patterns

### Testing Exceptions

```python
def test_exception_raised() -> None:
    """Test that exception is raised."""
    with pytest.raises(ValueError):
        my_function(-1)

def test_exception_with_message() -> None:
    """Test exception message matches pattern."""
    with pytest.raises(ValueError, match="must be positive"):
        my_function(-1)

def test_exception_attributes() -> None:
    """Test exception has expected attributes."""
    with pytest.raises(ValueError) as exc_info:
        my_function(-1)

    assert exc_info.value.args[0] == "Value must be positive"
    assert exc_info.type is ValueError
```

### Testing Async Code

```python
import pytest

@pytest.mark.asyncio
async def test_async_function() -> None:
    """Test async function."""
    result = await fetch_data()
    assert result is not None

@pytest.mark.asyncio
async def test_async_with_fixture(async_client) -> None:
    """Test async code with fixture."""
    response = await async_client.get("/api/data")
    assert response.status_code == 200
```

### Mocking and Patching

```python
from unittest.mock import Mock, patch, MagicMock
import pytest

def test_with_mock() -> None:
    """Test using mock objects."""
    # Create a mock
    mock_db = Mock()
    mock_db.get_user.return_value = {"id": 1, "name": "John"}

    # Use the mock
    user = mock_db.get_user(1)
    assert user["name"] == "John"

    # Verify mock was called
    mock_db.get_user.assert_called_once_with(1)

@patch("myproject.api.requests.get")
def test_with_patch(mock_get) -> None:
    """Test using patch decorator."""
    # Configure mock return value
    mock_response = Mock()
    mock_response.json.return_value = {"status": "ok"}
    mock_response.status_code = 200
    mock_get.return_value = mock_response

    # Call function that uses requests.get
    result = fetch_api_data()

    # Verify
    assert result["status"] == "ok"
    mock_get.assert_called_once()

def test_with_pytest_mock(mocker) -> None:
    """Test using pytest-mock plugin."""
    # Mock a method
    mock = mocker.patch("myproject.api.requests.get")
    mock.return_value.json.return_value = {"data": "test"}

    result = fetch_api_data()
    assert result["data"] == "test"
```

### Testing with Temporary Files

```python
import pytest
from pathlib import Path

def test_with_tmp_path(tmp_path: Path) -> None:
    """Test using temporary directory.

    tmp_path is a pytest fixture that provides a temporary directory
    unique to the test invocation.
    """
    # Create a file in temp directory
    test_file = tmp_path / "test.txt"
    test_file.write_text("test content")

    # Use the file
    result = process_file(test_file)

    assert result is not None
    assert test_file.exists()

def test_with_tmp_path_factory(tmp_path_factory) -> None:
    """Test using temporary directory factory.

    Useful for session-scoped fixtures.
    """
    temp_dir = tmp_path_factory.mktemp("data")
    test_file = temp_dir / "test.txt"
    test_file.write_text("content")

    assert test_file.read_text() == "content"
```

## Test Configuration

### pyproject.toml

```toml
[tool.pytest.ini_options]
testpaths = ["tests"]
python_files = "test_*.py"
python_classes = "Test*"
python_functions = "test_*"

# Add options
addopts = [
    "--strict-markers",     # Fail on unknown markers
    "--strict-config",      # Fail on config errors
    "--verbose",            # Verbose output
    "--cov=src",           # Coverage for src directory
    "--cov-report=term-missing",  # Show missing lines
    "--cov-report=html",   # Generate HTML report
    "--cov-fail-under=80", # Fail if coverage < 80%
]

# Markers for categorizing tests
markers = [
    "slow: marks tests as slow (deselect with '-m \"not slow\"')",
    "integration: marks tests as integration tests",
    "unit: marks tests as unit tests",
]

# Filter warnings
filterwarnings = [
    "error",  # Treat warnings as errors
    "ignore::DeprecationWarning",  # Ignore deprecation warnings
]
```

### Coverage Configuration

```toml
[tool.coverage.run]
source = ["src"]
omit = [
    "tests/*",
    "**/__pycache__/*",
    "**/site-packages/*",
]
branch = true  # Measure branch coverage

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
    "@abstractmethod",
]

[tool.coverage.html]
directory = "htmlcov"
```

## Test Markers

### Using Markers

```python
import pytest

@pytest.mark.slow
def test_slow_operation() -> None:
    """Test that takes a long time to run."""
    # ... slow test ...

@pytest.mark.integration
def test_api_integration() -> None:
    """Integration test with external API."""
    # ... integration test ...

@pytest.mark.unit
def test_unit_calculation() -> None:
    """Fast unit test."""
    # ... unit test ...

@pytest.mark.skip(reason="Not implemented yet")
def test_future_feature() -> None:
    """Test for future feature."""

@pytest.mark.skipif(sys.platform == "win32", reason="Unix only")
def test_unix_specific() -> None:
    """Test that only runs on Unix."""

@pytest.mark.xfail(reason="Known bug #123")
def test_known_failure() -> None:
    """Test expected to fail due to known bug."""
```

### Running Tests by Marker

```bash
# Run only unit tests
uv run pytest -m unit

# Run everything except slow tests
uv run pytest -m "not slow"

# Run integration tests only
uv run pytest -m integration

# Combine markers
uv run pytest -m "unit and not slow"
```

## Continuous Testing

### Watch Mode (with pytest-watch)

```bash
# Install pytest-watch
uv add --dev pytest-watch

# Watch for changes and re-run tests
uv run ptw

# Watch specific directory
uv run ptw tests/

# Watch with coverage
uv run ptw -- --cov
```

### Pre-commit Hook

```yaml
# .pre-commit-config.yaml
repos:
  - repo: local
    hooks:
      - id: pytest
        name: pytest
        entry: uv run pytest
        language: system
        pass_filenames: false
        always_run: true
```

## Common Test Commands

```bash
# Run tests with coverage and generate HTML report
uv run pytest --cov --cov-report=html

# Run only tests that failed last time
uv run pytest --lf

# Run tests in parallel (requires pytest-xdist)
uv add --dev pytest-xdist
uv run pytest -n auto

# Generate JUnit XML report (for CI)
uv run pytest --junit-xml=report.xml

# Profile slow tests (requires pytest-profiling)
uv add --dev pytest-profiling
uv run pytest --profile

# Debug tests with pdb
uv run pytest --pdb  # Drop into debugger on failure
uv run pytest -x --pdb  # Stop at first failure and debug
```

## Best Practices

1. **Organize tests by module** - Mirror your src structure in tests
2. **Use descriptive names** - Test names should describe what they test
3. **One assertion per test** - Keep tests focused (when practical)
4. **Use fixtures** - Share setup code across tests
5. **Parametrize** - Test multiple inputs with one test function
6. **Test edge cases** - Empty inputs, None, negative numbers, etc.
7. **Mock external dependencies** - Don't hit real APIs or databases
8. **Aim for >80% coverage** - But 100% coverage doesn't mean bug-free
9. **Keep tests fast** - Mark slow tests and run them separately
10. **Use markers** - Categorize tests for selective running
