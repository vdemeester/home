#!/usr/bin/env bash
set -euo pipefail

# Toggle or set color scheme
# Usage: toggle-color-scheme [light|dark]
# Without argument: toggles between light and dark
# With argument: sets to specified scheme

# Get current color scheme from dconf
current_scheme=$(dconf read /org/gnome/desktop/interface/color-scheme | tr -d "'")

# Determine new scheme
if [ $# -eq 0 ]; then
    # No argument: toggle
    if [[ "$current_scheme" == "prefer-dark" ]]; then
        new_scheme="prefer-light"
    else
        new_scheme="prefer-dark"
    fi
    echo "Toggling from $current_scheme to $new_scheme"
else
    # Argument provided: set to specific scheme
    case "$1" in
        light)
            new_scheme="prefer-light"
            ;;
        dark)
            new_scheme="prefer-dark"
            ;;
        *)
            echo "Invalid argument: $1"
            echo "Usage: $0 [light|dark]"
            exit 1
            ;;
    esac
    echo "Setting color scheme to $new_scheme"
fi

# Set new color scheme via dconf
dconf write /org/gnome/desktop/interface/color-scheme "'$new_scheme'"

# Trigger niri screen transition for smooth visual effect
if command -v niri &> /dev/null; then
    niri msg action do-screen-transition
fi

echo "Color scheme is now $new_scheme"
