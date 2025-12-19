#!/usr/bin/env nix-shell
#! nix-shell -i bash -p jq
# shellcheck shell=bash

# DetectRuntime.sh - Detect Docker or Podman runtime and return information
# Usage: DetectRuntime.sh [--runtime docker|podman] [--json]

set -euo pipefail

show_usage() {
    cat <<EOF
Usage: DetectRuntime.sh [OPTIONS]

Detect container runtime (Docker or Podman) and return information.

OPTIONS:
    --runtime RUNTIME    Force specific runtime (docker|podman)
    --json              Output in JSON format
    -h, --help          Show this help message

EXAMPLES:
    DetectRuntime.sh                    # Auto-detect available runtime
    DetectRuntime.sh --runtime docker   # Force Docker runtime
    DetectRuntime.sh --json             # Output as JSON
EOF
}

detect_runtime() {
    local force_runtime=""
    local output_json=false

    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            --runtime)
                force_runtime="$2"
                shift 2
                ;;
            --json)
                output_json=true
                shift
                ;;
            -h|--help)
                show_usage
                exit 0
                ;;
            *)
                echo "ERROR: Unknown option: $1" >&2
                show_usage >&2
                exit 1
                ;;
        esac
    done

    local runtime=""
    local compose=""
    local version=""
    local rootless="false"
    local socket=""

    # If specific runtime requested, check only that one
    if [[ -n "$force_runtime" ]]; then
        case "$force_runtime" in
            podman)
                if ! command -v podman &>/dev/null; then
                    echo "ERROR: Podman requested but not found in PATH" >&2
                    exit 1
                fi
                runtime="podman"
                ;;
            docker)
                if ! command -v docker &>/dev/null; then
                    echo "ERROR: Docker requested but not found in PATH" >&2
                    exit 1
                fi
                runtime="docker"
                ;;
            *)
                echo "ERROR: Invalid runtime '$force_runtime'. Must be 'docker' or 'podman'" >&2
                exit 1
                ;;
        esac
    fi

    # Auto-detect if no runtime forced
    if [[ -z "$runtime" ]]; then
        # Check for Podman first (preference for rootless)
        if command -v podman &>/dev/null; then
            runtime="podman"
        # Fall back to Docker
        elif command -v docker &>/dev/null; then
            runtime="docker"
        else
            echo "ERROR: Neither Docker nor Podman found in PATH" >&2
            exit 1
        fi
    fi

    # Get runtime-specific information
    case "$runtime" in
        podman)
            version=$(podman --version | awk '{print $3}')

            # Check if running rootless
            if podman info --format '{{.Host.Security.Rootless}}' 2>/dev/null | grep -q "true"; then
                rootless="true"
                socket="unix://$XDG_RUNTIME_DIR/podman/podman.sock"
            else
                rootless="false"
                socket="unix:///run/podman/podman.sock"
            fi

            # Check for podman-compose
            if command -v podman-compose &>/dev/null; then
                compose="podman-compose"
            elif command -v docker-compose &>/dev/null; then
                compose="docker-compose"
            else
                compose="none"
            fi
            ;;
        docker)
            version=$(docker --version | awk '{print $3}' | tr -d ',')
            rootless="false"
            socket="unix:///var/run/docker.sock"

            # Check for docker-compose
            if docker compose version &>/dev/null 2>&1; then
                compose="docker-compose-plugin"
            elif command -v docker-compose &>/dev/null; then
                compose="docker-compose"
            else
                compose="none"
            fi
            ;;
    esac

    # Output format
    if [[ "$output_json" == true ]]; then
        jq -n \
            --arg runtime "$runtime" \
            --arg version "$version" \
            --arg compose "$compose" \
            --arg rootless "$rootless" \
            --arg socket "$socket" \
            '{
                runtime: $runtime,
                version: $version,
                compose: $compose,
                rootless: ($rootless == "true"),
                socket: $socket
            }'
    else
        echo "Runtime: $runtime"
        echo "Version: $version"
        echo "Compose: $compose"
        echo "Rootless: $rootless"
        echo "Socket: $socket"
    fi
}

detect_runtime "$@"
