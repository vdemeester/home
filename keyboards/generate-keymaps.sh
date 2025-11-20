#! /usr/bin/env nix-shell
#! nix-shell -i bash -p qmk -p keymap-drawer
# shellcheck shell=bash

# Generate keymap SVGs for keyboards using keymap-drawer
# Usage: ./generate-keymaps.sh [eyelash|moonlander|all]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
KEYMAP_DRAWER_DIR="$SCRIPT_DIR/keymap-drawer"
OUTPUT_DIR="${OUTPUT_DIR:-$SCRIPT_DIR}"

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
	echo -e "${BLUE}==>${NC} $1"
}

log_success() {
	echo -e "${GREEN}==>${NC} $1"
}

log_warn() {
	echo -e "${YELLOW}==>${NC} $1"
}

# Create output directory
mkdir -p "$OUTPUT_DIR"

generate_eyelash_corne() {
	log_info "Generating keymap for eyelash_corne (ZMK)..."

	local zmk_config="$SCRIPT_DIR/eyelash_corne/config/eyelash_corne.keymap"
	local config_yaml="$KEYMAP_DRAWER_DIR/config.yaml"
	local output_svg="$OUTPUT_DIR/eyelash_corne.svg"

	if [[ ! -f "$zmk_config" ]]; then
		log_warn "ZMK config not found at $zmk_config"
		return 1
	fi

	# Parse ZMK keymap and draw SVG
	keymap -c "$config_yaml" parse -z "$zmk_config" |
		keymap -c "$config_yaml" draw - >"$output_svg"

	log_success "Generated: $output_svg"
}

generate_moonlander() {
	log_info "Generating keymap for moonlander (QMK)..."

	local qmk_firmware_dir="$SCRIPT_DIR/moonlander/build/qmk_firmware"
	local keyboard="zsa/moonlander"
	local keymap_name="vincent"
	local qmk_json="/tmp/moonlander_keymap.json"
	local output_svg="$OUTPUT_DIR/moonlander.svg"
	local config_yaml="$KEYMAP_DRAWER_DIR/config.yaml"

	# Check if QMK firmware is checked out
	if [[ ! -d "$qmk_firmware_dir" ]]; then
		log_warn "QMK firmware not found. Running checkout..."
		(cd "$SCRIPT_DIR/moonlander" && ./go.sh checkout)
	fi

	# Ensure symlink exists
	if [[ ! -L "$qmk_firmware_dir/keyboards/$keyboard/keymaps/$keymap_name" ]]; then
		log_info "Creating symlink for keymap..."
		mkdir -p "$qmk_firmware_dir/keyboards/$keyboard/keymaps"
		ln -rvsf "$SCRIPT_DIR/moonlander/config" "$qmk_firmware_dir/keyboards/$keyboard/keymaps/$keymap_name"
	fi

	# Convert keymap to JSON using QMK CLI from the firmware directory
	log_info "Converting QMK keymap to JSON..."
	if ! (cd "$qmk_firmware_dir" && qmk c2json --no-cpp -kb "$keyboard" -km "$keymap_name" >"$qmk_json" 2>/dev/null); then
		log_warn "QMK conversion failed."
		log_warn "Alternative: Use QMK Configurator to export JSON or manually create YAML."
		return 1
	fi

	# Parse QMK JSON and draw SVG
	keymap parse -c 14 -q "$qmk_json" |
		keymap -c "$config_yaml" draw - >"$output_svg"

	log_success "Generated: $output_svg"

	# Clean up temp file
	rm -f "$qmk_json"
}

# Main logic
case "${1:-all}" in
eyelash | eyelash_corne)
	generate_eyelash_corne
	;;
moonlander)
	generate_moonlander
	;;
all)
	generate_eyelash_corne || true
	generate_moonlander || true
	;;
*)
	echo "Usage: $0 [eyelash|moonlander|all]"
	echo ""
	echo "Options:"
	echo "  eyelash     Generate only eyelash_corne keymap"
	echo "  moonlander  Generate only moonlander keymap"
	echo "  all         Generate all keymaps (default)"
	echo ""
	echo "Output directory: $OUTPUT_DIR"
	exit 1
	;;
esac

log_success "Done! SVGs saved to: $OUTPUT_DIR"
