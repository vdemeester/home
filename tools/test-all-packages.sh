#!/usr/bin/env bash
# test-all-packages.sh - Test all custom packages
#
# Usage: ./test-all-packages.sh

set -e

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# List of all packages to test
PACKAGES=(
  # Go packages
  "ape"
  "batzconverter"
  "govanityurl"
  "manifest-tool"
  "ram"
  "claude-hooks"
  "arr"
  "download-kiwix-zim"
  "gh-restart-failed"

  # Shell script packages
  "scripts"
  "vrsync"
  "vde-thinkpad"

  # System packages
  "systemd-email"
  "battery-monitor"

  # Other packages
  "homepage"
  "bookmark-plus"
)

PASSED=0
FAILED=0
FAILED_PACKAGES=()

echo "=========================================="
echo "Testing ${#PACKAGES[@]} packages"
echo "=========================================="
echo ""

for PKG in "${PACKAGES[@]}"; do
  echo "[$((PASSED + FAILED + 1))/${#PACKAGES[@]}] Testing: $PKG"
  if "$SCRIPT_DIR/test-package.sh" "$PKG" > "/tmp/test-$PKG.log" 2>&1; then
    echo "  ✓ PASSED"
    PASSED=$((PASSED + 1))
  else
    echo "  ✗ FAILED (see /tmp/test-$PKG.log)"
    FAILED=$((FAILED + 1))
    FAILED_PACKAGES+=("$PKG")
  fi
  echo ""
done

echo "=========================================="
echo "Test Summary"
echo "=========================================="
echo "Passed: $PASSED"
echo "Failed: $FAILED"

if [ $FAILED -gt 0 ]; then
  echo ""
  echo "Failed packages:"
  for PKG in "${FAILED_PACKAGES[@]}"; do
    echo "  - $PKG (log: /tmp/test-$PKG.log)"
  done
  exit 1
fi

echo ""
echo "All packages passed!"
