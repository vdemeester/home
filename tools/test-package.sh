#!/usr/bin/env bash
# test-package.sh - Enhanced package testing
#
# Usage: ./test-package.sh <package-name>

set -e

PACKAGE=$1

if [ -z "$PACKAGE" ]; then
  echo "Usage: $0 <package-name>"
  exit 1
fi

echo "=========================================="
echo "Testing package: $PACKAGE"
echo "=========================================="

# 1. Build
echo ""
echo "==> Building..."
if ! nix build .#"$PACKAGE" --no-link 2>&1 | tail -5; then
  echo "ERROR: Build failed"
  exit 1
fi
echo "✓ Build successful"

# 2. Check meta
echo ""
echo "==> Checking metadata..."
META=$(nix eval .#"$PACKAGE".meta --json 2>/dev/null || echo '{}')

# Check description
if ! echo "$META" | jq -e '.description' > /dev/null 2>&1; then
  echo "ERROR: Missing description"
  exit 1
fi
DESCRIPTION=$(echo "$META" | jq -r '.description')
echo "  Description: $DESCRIPTION"

# Check license
if ! echo "$META" | jq -e '.license' > /dev/null 2>&1; then
  echo "WARNING: Missing license"
else
  LICENSE=$(echo "$META" | jq -r '.license.spdxId // .license.shortName // .license')
  echo "  License: $LICENSE"
fi

# Check platforms
if ! echo "$META" | jq -e '.platforms' > /dev/null 2>&1; then
  echo "WARNING: Missing platforms"
else
  PLATFORM_COUNT=$(echo "$META" | jq '.platforms | length')
  echo "  Platforms: $PLATFORM_COUNT supported"
fi

# Check mainProgram
if echo "$META" | jq -e '.mainProgram' > /dev/null 2>&1; then
  MAIN_PROGRAM=$(echo "$META" | jq -r '.mainProgram')
  echo "  Main program: $MAIN_PROGRAM"
fi

echo "✓ Metadata complete"

# 3. Test binary (if executable)
echo ""
echo "==> Testing binary..."
BUILD_RESULT=$(nix build .#"$PACKAGE" --no-link --print-out-paths 2>/dev/null)

if [ -d "$BUILD_RESULT/bin" ]; then
  BINARIES=$(ls "$BUILD_RESULT/bin")
  echo "  Found binaries: $BINARIES"

  for BIN in $BINARIES; do
    BIN_PATH="$BUILD_RESULT/bin/$BIN"
    if [ -x "$BIN_PATH" ]; then
      echo "  Testing: $BIN"

      # Try --version (with timeout)
      if timeout 2s "$BIN_PATH" --version >/dev/null 2>&1; then
        VERSION=$(timeout 2s "$BIN_PATH" --version 2>&1 | head -1)
        echo "    --version: $VERSION"
      # Try --help (with timeout)
      elif timeout 2s "$BIN_PATH" --help >/dev/null 2>&1; then
        echo "    --help: Available"
      # Try -h (with timeout)
      elif timeout 2s "$BIN_PATH" -h >/dev/null 2>&1; then
        echo "    -h: Available"
      else
        echo "    WARNING: No --version, --help, or -h support"
      fi
    fi
  done
  echo "✓ Binary tests completed"
else
  echo "  No binaries found (library or data package)"
fi

# 4. Check dependencies
echo ""
echo "==> Checking dependencies..."
DEP_COUNT=$(nix path-info -r .#"$PACKAGE" 2>/dev/null | wc -l)
echo "  Runtime dependencies: $DEP_COUNT packages"

# Show largest dependencies
echo "  Largest dependencies:"
nix path-info -rsSh .#"$PACKAGE" 2>/dev/null | sort -hr | head -5 | sed 's/^/    /'

echo "✓ Dependencies checked"

# 5. Test in clean shell
echo ""
echo "==> Testing in clean environment..."
if [ -d "$BUILD_RESULT/bin" ]; then
  for BIN in "$BUILD_RESULT"/bin/*; do
    BIN_NAME=$(basename "$BIN")
    if nix shell .#"$PACKAGE" --command which "$BIN_NAME" >/dev/null 2>&1; then
      echo "  ✓ $BIN_NAME available in PATH"
    else
      echo "  ERROR: $BIN_NAME not found in PATH"
      exit 1
    fi
  done
else
  echo "  Skipping (no binaries)"
fi

echo ""
echo "=========================================="
echo "All tests passed for $PACKAGE!"
echo "=========================================="
