# Package Workflow

Build and publish Python packages.

## When to Use

- "build python package"
- "publish to pypi"
- "create wheel"
- "distribute package"

## Quick Commands

```bash
# Build package
uv build

# Build wheel only
uv build --wheel

# Build source distribution only
uv build --sdist

# Publish to PyPI
uv publish

# Publish to test PyPI
uv publish --publish-url https://test.pypi.org/legacy/
```

## Package Configuration

### pyproject.toml

```toml
[project]
name = "mypackage"
version = "0.1.0"
description = "My awesome package"
readme = "README.md"
requires-python = ">=3.11"
license = {text = "MIT"}
authors = [
    {name = "Your Name", email = "you@example.com"}
]
keywords = ["example", "package"]
classifiers = [
    "Development Status :: 3 - Alpha",
    "Intended Audience :: Developers",
    "License :: OSI Approved :: MIT License",
    "Programming Language :: Python :: 3.11",
]

dependencies = [
    "requests>=2.31.0",
]

[project.urls]
Homepage = "https://github.com/user/mypackage"
Repository = "https://github.com/user/mypackage"
Documentation = "https://mypackage.readthedocs.io"
Changelog = "https://github.com/user/mypackage/blob/main/CHANGELOG.md"

[project.scripts]
mypackage = "mypackage.cli:main"

[build-system]
requires = ["hatchling"]
build-backend = "hatchling.build"

[tool.hatch.build.targets.wheel]
packages = ["src/mypackage"]
```

## Building

```bash
# Clean previous builds
rm -rf dist/

# Build distributions
uv build

# Outputs:
# dist/mypackage-0.1.0-py3-none-any.whl  (wheel)
# dist/mypackage-0.1.0.tar.gz            (source)
```

## Publishing

### Setup PyPI Credentials

```bash
# Set token (recommended)
export UV_PUBLISH_TOKEN=pypi-...

# Or use ~/.pypirc
cat > ~/.pypirc << EOF
[pypi]
username = __token__
password = pypi-...
EOF
```

### Publish

```bash
# Publish to PyPI
uv publish

# Publish to Test PyPI
uv publish --publish-url https://test.pypi.org/legacy/

# Dry run
uv publish --dry-run
```

## Version Management

Update version in `pyproject.toml`, then:

```bash
# Tag release
git tag v0.1.0
git push --tags

# Build and publish
uv build
uv publish
```

## Best Practices

1. **Use src layout** - `src/package/`
2. **Semantic versioning** - MAJOR.MINOR.PATCH
3. **Include README** - And CHANGELOG
4. **Test on Test PyPI** - Before real PyPI
5. **Use build backend** - hatchling, setuptools, etc.
6. **Add classifiers** - Help users find your package
