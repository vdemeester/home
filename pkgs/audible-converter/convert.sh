#!/usr/bin/env bash
# Audible to Audiobookshelf converter
# Downloads audiobooks from Audible and converts them to M4B format

set -euo pipefail

# Configuration
TEMP_DIR="${AUDIBLE_TEMP_DIR:-/tmp/audible-download}"
OUTPUT_DIR="${AUDIBLE_OUTPUT_DIR:-$HOME/audiobooks}"
QUALITY="${AUDIBLE_QUALITY:-best}"
FORMAT="${AUDIBLE_FORMAT:-m4b}"
AUTHCODE="${AUDIBLE_AUTHCODE:-}"
CLEANUP_ON_EXIT="${AUDIBLE_CLEANUP_ON_EXIT:-false}" # Set to 'true' to auto-cleanup temp files

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
	echo -e "${GREEN}[INFO]${NC} $*"
}

log_warn() {
	echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
	echo -e "${RED}[ERROR]${NC} $*"
}

# Usage information
usage() {
	cat <<EOF
Usage: $(basename "$0") [OPTIONS] [COMMAND]

Audible to Audiobookshelf converter - Downloads and converts audiobooks

COMMANDS:
    download-all    Download all books from Audible library
    download ASIN   Download specific book by ASIN
    convert FILE    Convert existing AAX file to M4B
    sync            Download and convert all new books (default)
    list            List Audible library

OPTIONS:
    -o, --output DIR     Output directory (default: \$HOME/audiobooks)
    -t, --temp DIR       Temporary download directory (default: /tmp/audible-download)
    -q, --quality QUAL   Audio quality: best, high, normal (default: $QUALITY)
    -f, --format FMT     Output format: m4b, mp3, m4a (default: $FORMAT)
    -a, --authcode CODE  Audible activation bytes for AAX DRM removal
    -h, --help           Show this help message

ENVIRONMENT VARIABLES:
    AUDIBLE_OUTPUT_DIR      Output directory for converted books
    AUDIBLE_TEMP_DIR        Temporary directory for downloads (kept by default)
    AUDIBLE_QUALITY         Audio quality setting
    AUDIBLE_FORMAT          Output format
    AUDIBLE_AUTHCODE        Activation bytes for AAX DRM removal
    AUDIBLE_CLEANUP_ON_EXIT Set to 'true' to auto-delete temp files on exit

EXAMPLES:
    # Sync library (download and convert new books)
    $(basename "$0") sync

    # Download specific book
    $(basename "$0") download B01234567X

    # Convert existing AAX file
    $(basename "$0") convert /path/to/book.aax

    # Download all books to custom directory
    $(basename "$0") --output /mnt/audiobooks download-all

EOF
}

# Check dependencies
check_dependencies() {
	local deps=("audible" "aaxtomp3" "ffmpeg" "mediainfo" "jq")
	local missing=()

	for dep in "${deps[@]}"; do
		if ! command -v "$dep" &>/dev/null; then
			missing+=("$dep")
		fi
	done

	if [ ${#missing[@]} -ne 0 ]; then
		log_error "Missing required dependencies: ${missing[*]}"
		log_error "Please install: nix-shell -p audible-cli aaxtomp3 ffmpeg mediainfo jq"
		exit 1
	fi
}

# Check if authenticated with Audible
check_auth() {
	if ! audible library list &>/dev/null; then
		log_error "Not authenticated with Audible"
		log_error "Please run: audible quickstart"
		exit 1
	fi
}

# Create directories
setup_dirs() {
	mkdir -p "$TEMP_DIR"
	mkdir -p "$OUTPUT_DIR"
}

# Cleanup temporary files (manual or via AUDIBLE_CLEANUP_ON_EXIT=true)
# By default, we keep temp files to reuse downloaded AAX files on subsequent runs
cleanup() {
	if [ -d "$TEMP_DIR" ]; then
		log_info "Cleaning up temporary files in: $TEMP_DIR"
		rm -rf "$TEMP_DIR"
		log_info "Cleanup complete"
	fi
}

# Download entire library
download_all() {
	log_info "Exporting library metadata..."
	local library_json="$TEMP_DIR/library.json"

	audible library export --format json --output "$library_json"

	local total_books
	total_books=$(jq length "$library_json")
	log_info "Found $total_books books in library"

	local count=0
	jq -r '.[].asin' "$library_json" | while read -r asin; do
		count=$((count + 1))
		log_info "Downloading book $count/$total_books (ASIN: $asin)..."

		if ! audible download \
			--asin "$asin" \
			--aax-fallback \
			--chapter \
			--annotation \
			--pdf \
			--cover \
			--quality "$QUALITY" \
			--ignore-podcasts \
			--output-dir "$TEMP_DIR" 2>&1; then
			log_warn "Failed to download ASIN: $asin (may already be downloaded)"
		fi
	done
}

# Download specific book by ASIN
download_book() {
	local asin="$1"
	log_info "Downloading book ASIN: $asin..."

	audible download \
		--asin "$asin" \
		--aax \
		--quality "$QUALITY" \
		--ignore-podcasts \
		--output-dir "$TEMP_DIR"
}

# Convert AAX files to M4B
# Usage: convert_books [directory|file]
convert_books() {
	local source_path="${1:-$TEMP_DIR}"
	local aax_files=()

	# Determine if we're converting a directory or a single file
	if [ -d "$source_path" ]; then
		# Find all AAX files in directory using find
		log_info "Searching for AAX files in: $source_path"
		mapfile -t aax_files < <(find "$source_path" -maxdepth 1 -type f -name "*.aax" -o -name "*.AAX")

		if [ ${#aax_files[@]} -eq 0 ]; then
			log_warn "No AAX files found in $source_path"
			return 0
		fi
		log_info "Found ${#aax_files[@]} AAX file(s)"
	elif [ -f "$source_path" ]; then
		# Single file
		aax_files=("$source_path")
	else
		log_error "Invalid path: $source_path"
		return 1
	fi

	local total_files=${#aax_files[@]}
	log_info "Converting $total_files AAX file(s) to $FORMAT format..."

	# Get authcode (either provided or from audible-cli)
	local authcode="$AUTHCODE"
	if [ -z "$authcode" ]; then
		# Try to get activation bytes from audible-cli
		log_info "Attempting to retrieve activation bytes from audible-cli..."
		if authcode=$(audible activation-bytes 2>/dev/null); then
			if [ -n "$authcode" ]; then
				log_info "Using activation bytes from audible-cli: $authcode"
			else
				log_warn "Activation bytes command succeeded but returned empty value"
				authcode=""
			fi
		else
			log_warn "Could not retrieve activation bytes from audible-cli"
			authcode=""
		fi
	fi

	# Base aaxtomp3 options (no --batch, we'll loop instead)
	local aaxtomp3_opts=(--target_dir "$OUTPUT_DIR" --no-clobber)

	# Add authcode if available, otherwise use audible-cli data
	if [ -n "$authcode" ]; then
		aaxtomp3_opts+=(--authcode "$authcode")
	else
		log_warn "No authcode available, trying --use-audible-cli-data flag"
		aaxtomp3_opts+=(--use-audible-cli-data)
	fi

	case "$FORMAT" in
	m4b)
		aaxtomp3_opts+=(--single)
		;;
	mp3)
		aaxtomp3_opts+=(--codec libmp3lame)
		;;
	m4a)
		# Default M4A output
		;;
	*)
		log_error "Unknown format: $FORMAT"
		exit 1
		;;
	esac

	# Convert each file individually (--batch doesn't work with --authcode)
	# Note: --no-clobber flag makes aaxtomp3 skip existing files automatically
	local converted=0
	local failed=0
	local current=0

	log_info "Starting conversion loop for ${#aax_files[@]} files..."

	for aax_file in "${aax_files[@]}"; do
		local base_filename
		base_filename=$(basename "$aax_file")
		current=$((current + 1))

		log_info "[$current/$total_files] Converting: $base_filename"
		echo "----------------------------------------"

		# Run aaxtomp3 directly (let output flow to terminal)
		# Temporarily disable exit-on-error for this command
		set +e
		aaxtomp3 "${aaxtomp3_opts[@]}" "$aax_file"
		local exit_code=$?
		set -e

		if [ $exit_code -eq 0 ]; then
			converted=$((converted + 1))
			log_info "✓ Successfully converted: $base_filename"
		else
			failed=$((failed + 1))
			log_warn "✗ Failed to convert: $base_filename (exit code: $exit_code)"
		fi
		echo ""
	done

	# Report summary
	local skipped=$((total_files - converted - failed))
	log_info "Conversion complete!"
	log_info "  Converted: $converted/$total_files"
	if [ $skipped -gt 0 ]; then
		log_info "  Skipped: $skipped/$total_files (already existed)"
	fi
	if [ $failed -gt 0 ]; then
		log_warn "  Failed: $failed/$total_files"
		log_error ""
		log_error "If you got 'Missing authcode' errors, follow these steps:"
		log_error "  1. Authenticate with Audible:"
		log_error "     audible quickstart"
		log_error ""
		log_error "  2. Verify activation bytes are available:"
		log_error "     audible activation-bytes"
		log_error ""
		log_error "  3. Alternatively, provide authcode manually:"
		log_error "     --authcode YOUR_ACTIVATION_BYTES"
		log_error "     or: export AUDIBLE_AUTHCODE=YOUR_ACTIVATION_BYTES"
		exit 1
	fi
	log_info "Books saved to: $OUTPUT_DIR"
}

# List Audible library
list_library() {
	log_info "Fetching Audible library..."
	audible library list | jq -r '.[] | "\(.title) by \(.authors) (ASIN: \(.asin))"'
}

# Sync library (download and convert new books)
sync_library() {
	log_info "Syncing Audible library..."
	download_all
	convert_books
}

# Main function
main() {
	local command="sync"
	local -a positional_args=()

	# Parse arguments
	while [[ $# -gt 0 ]]; do
		case $1 in
		-o | --output)
			OUTPUT_DIR="$2"
			shift 2
			;;
		-t | --temp)
			TEMP_DIR="$2"
			shift 2
			;;
		-q | --quality)
			QUALITY="$2"
			shift 2
			;;
		-f | --format)
			FORMAT="$2"
			shift 2
			;;
		-a | --authcode)
			AUTHCODE="$2"
			shift 2
			;;
		-h | --help)
			usage
			exit 0
			;;
		download-all | download | convert | sync | list)
			command="$1"
			shift
			;;
		-*)
			log_error "Unknown option: $1"
			usage
			exit 1
			;;
		*)
			# Collect positional arguments (file paths, ASINs, etc.)
			positional_args+=("$1")
			shift
			;;
		esac
	done

	# Optionally cleanup on exit (default: keep files for reuse)
	if [ "$CLEANUP_ON_EXIT" = "true" ]; then
		log_info "Auto-cleanup enabled (AUDIBLE_CLEANUP_ON_EXIT=true)"
		trap cleanup EXIT
	else
		log_info "Keeping temp files in $TEMP_DIR for reuse (set AUDIBLE_CLEANUP_ON_EXIT=true to auto-delete)"
	fi

	# Check dependencies
	check_dependencies

	# Setup directories
	setup_dirs

	# Execute command
	case $command in
	download-all)
		check_auth
		download_all
		;;
	download)
		if [ ${#positional_args[@]} -eq 0 ]; then
			log_error "ASIN required for download command"
			usage
			exit 1
		fi
		check_auth
		download_book "${positional_args[0]}"
		;;
	convert)
		if [ ${#positional_args[@]} -eq 0 ]; then
			log_error "File or directory path required for convert command"
			usage
			exit 1
		fi
		# Get the last positional argument as the path
		local convert_path="${positional_args[-1]}"
		if [ ! -e "$convert_path" ]; then
			log_error "Path not found: $convert_path"
			exit 1
		fi

		# If it's a directory, convert all AAX files in it
		# If it's a file, convert just that specific file
		if [ -d "$convert_path" ]; then
			log_info "Converting all AAX files in directory: $convert_path"
			convert_books "$convert_path"
		elif [ -f "$convert_path" ]; then
			log_info "Converting single file: $(basename "$convert_path")"
			convert_books "$convert_path"
		else
			log_error "Path must be a file or directory: $convert_path"
			exit 1
		fi
		;;
	sync)
		check_auth
		sync_library
		;;
	list)
		check_auth
		list_library
		;;
	*)
		log_error "Unknown command: $command"
		usage
		exit 1
		;;
	esac
}

# Run main function
main "$@"
