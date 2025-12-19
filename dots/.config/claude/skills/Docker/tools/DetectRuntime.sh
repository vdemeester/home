#!/usr/bin/env nix-shell
#! nix-shell -i bash -p jq
# shellcheck shell=bash

# DetectRuntime.sh - Detect Docker or Podman runtime and return information
# Usage: DetectRuntime.sh [--json]

set -euo pipefail

detect_runtime() {
    local runtime=""
    local compose=""
    local version=""
    local rootless="false"
    local socket=""

    # Check for Podman first (preference for rootless)
    if command -v podman &>/dev/null; then
        runtime="podman"
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
    # Fall back to Docker
    elif command -v docker &>/dev/null; then
        runtime="docker"
        version=$(docker --version | awk '{print $3}' | tr -d ',')
        rootless="false"
        socket="unix:///var/run/docker.sock"

        # Check for docker-compose
        if docker compose version &>/dev/null; then
            compose="docker-compose-plugin"
        elif command -v docker-compose &>/dev/null; then
            compose="docker-compose"
        else
            compose="none"
        fi
    else
        echo "ERROR: Neither Docker nor Podman found in PATH" >&2
        exit 1
    fi

    # Output format
    if [[ "${1:-}" == "--json" ]]; then
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
