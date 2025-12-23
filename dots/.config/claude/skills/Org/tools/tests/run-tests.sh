#!/usr/bin/env bash
# run-tests.sh - Run ERT tests for org-manager batch functions
# Copyright (C) 2025 Vincent Demeester
# Part of Claude Code Org skill

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOOLS_DIR="$(dirname "$SCRIPT_DIR")"
TEST_FILE="$SCRIPT_DIR/batch-functions-test.el"
EMACS="${EMACS:-emacs}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

usage() {
    cat <<EOF
Usage: $0 [options]

Run ERT tests for org-manager batch functions.

Options:
  -v, --verbose     Verbose output (show all test details)
  -s, --selector    Run specific test selector (e.g., "test-org-batch-list-todos")
  -h, --help        Show this help message

Examples:
  # Run all tests
  $0

  # Run specific test
  $0 -s test-org-batch-list-todos

  # Verbose output
  $0 -v

Environment:
  EMACS            Path to emacs binary (default: emacs)

EOF
    exit 0
}

# Parse arguments
VERBOSE=0
SELECTOR=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        -v|--verbose)
            VERBOSE=1
            shift
            ;;
        -s|--selector)
            SELECTOR="$2"
            shift 2
            ;;
        -h|--help)
            usage
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            usage
            ;;
    esac
done

# Check dependencies
if ! command -v "$EMACS" &> /dev/null; then
    echo -e "${RED}Error: Emacs not found${NC}"
    echo "Set EMACS environment variable or install emacs"
    exit 1
fi

if [[ ! -f "$TEST_FILE" ]]; then
    echo -e "${RED}Error: Test file not found: $TEST_FILE${NC}"
    exit 1
fi

echo -e "${YELLOW}Running org-manager tests...${NC}"
echo "Test file: $TEST_FILE"
echo "Emacs: $EMACS"

if [[ -n "$SELECTOR" ]]; then
    echo "Selector: $SELECTOR"
fi
echo ""

# Build emacs command
EMACS_CMD=(
    "$EMACS"
    --batch
    --no-init-file
    -L "$TOOLS_DIR"
    -l "$TOOLS_DIR/batch-functions.el"
    -l "$TEST_FILE"
)

if [[ -n "$SELECTOR" ]]; then
    # Run specific test
    EMACS_CMD+=(--eval "(ert-run-tests-batch-and-exit '$SELECTOR)")
else
    # Run all tests
    EMACS_CMD+=(-f ert-run-tests-batch-and-exit)
fi

# Run tests
if [[ "$VERBOSE" == "1" ]]; then
    "${EMACS_CMD[@]}" 2>&1
    EXIT_CODE=$?
else
    # Capture output and show summary
    OUTPUT=$("${EMACS_CMD[@]}" 2>&1)
    EXIT_CODE=$?

    # Show summary
    echo "$OUTPUT" | tail -20
fi

echo ""
if [[ $EXIT_CODE -eq 0 ]]; then
    echo -e "${GREEN}✓ All tests passed!${NC}"
else
    echo -e "${RED}✗ Some tests failed${NC}"
    if [[ "$VERBOSE" == "0" ]]; then
        echo ""
        echo "Run with -v for detailed output"
    fi
fi

exit $EXIT_CODE
