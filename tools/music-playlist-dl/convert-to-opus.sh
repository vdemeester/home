#!/usr/bin/env nix-shell
#! nix-shell -i bash -p ffmpeg python3Packages.pyyaml
# shellcheck shell=bash
# Convert existing music downloads to Opus format
# This allows switching to Opus without re-downloading everything

set -euo pipefail

# Configuration
MUSIC_DIR="${MUSIC_DIR:-/neo/music}"
LIBRARY_DIR="${MUSIC_DIR}/library"
PLAYLIST_DIR="${MUSIC_DIR}/playlist"
OPUS_BITRATE="${OPUS_BITRATE:-128k}"
DRY_RUN="${DRY_RUN:-false}"
PARALLEL_JOBS="${PARALLEL_JOBS:-4}"
CONFIG_FILE="${CONFIG_FILE:-}"
CONFIG_ONLY="${CONFIG_ONLY:-false}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Statistics
TOTAL_FILES=0
CONVERTED=0
SKIPPED=0
FAILED=0
SPACE_SAVED=0

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*" >&2
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*" >&2
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*" >&2
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*" >&2
}

check_dependencies() {
    # Dependencies are provided by nix-shell shebang
    if ! command -v ffmpeg &> /dev/null; then
        log_error "ffmpeg not available"
        exit 1
    fi

    if ! command -v python3 &> /dev/null; then
        log_error "python3 not available"
        exit 1
    fi

    # Check Python YAML module
    if ! python3 -c "import yaml" 2>/dev/null; then
        log_error "Python yaml module not available"
        log_error "This should be provided by nix-shell"
        exit 1
    fi

    log_info "Dependencies OK (ffmpeg + python3-yaml from nix-shell)"
}

human_readable_size() {
    local bytes=$1
    if [ "$bytes" -lt 1024 ]; then
        echo "${bytes}B"
    elif [ "$bytes" -lt 1048576 ]; then
        echo "$((bytes / 1024))KB"
    elif [ "$bytes" -lt 1073741824 ]; then
        echo "$((bytes / 1048576))MB"
    else
        echo "$((bytes / 1073741824))GB"
    fi
}

convert_file() {
    local input_file="$1"

    # Skip empty filenames
    if [ -z "$input_file" ]; then
        log_warn "Skipping empty filename"
        SKIPPED=$((SKIPPED + 1))
        return 0
    fi

    local output_file="${input_file%.*}.opus"

    # Skip if output already exists
    if [ -f "$output_file" ]; then
        log_warn "Already exists, skipping: $output_file"
        SKIPPED=$((SKIPPED + 1))
        return 0
    fi

    if [ "$DRY_RUN" = "true" ]; then
        log_info "[DRY-RUN] Would convert: $input_file"
        CONVERTED=$((CONVERTED + 1))
        return 0
    fi

    local input_size
    input_size=$(stat -f%z "$input_file" 2>/dev/null || stat -c%s "$input_file" 2>/dev/null)

    log_info "Converting: $(basename "$input_file")"

    # Convert with ffmpeg, preserving metadata and cover art
    if ffmpeg -i "$input_file" \
        -vn \
        -c:a libopus \
        -b:a "$OPUS_BITRATE" \
        -map_metadata 0 \
        -map 0:a \
        -id3v2_version 3 \
        -f opus \
        "$output_file.tmp" \
        -loglevel error -stats 2>&1; then

        # Move temp file to final location
        mv "$output_file.tmp" "$output_file"

        # Preserve timestamps
        touch -r "$input_file" "$output_file"

        local output_size
        output_size=$(stat -f%z "$output_file" 2>/dev/null || stat -c%s "$output_file" 2>/dev/null)
        local saved=$((input_size - output_size))
        SPACE_SAVED=$((SPACE_SAVED + saved))

        log_success "Converted: $(basename "$output_file") (saved $(human_readable_size "$saved"))"

        # Remove original file
        rm "$input_file"

        CONVERTED=$((CONVERTED + 1))
        return 0
    else
        log_error "Failed to convert: $input_file"
        rm -f "$output_file.tmp"
        FAILED=$((FAILED + 1))
        return 1
    fi
}

update_playlists() {
    if [ "$DRY_RUN" = "true" ]; then
        log_info "[DRY-RUN] Would update playlists in: $PLAYLIST_DIR"
        return 0
    fi

    if [ ! -d "$PLAYLIST_DIR" ]; then
        log_warn "Playlist directory not found: $PLAYLIST_DIR"
        return 0
    fi

    log_info "Updating playlists..."

    local playlist_count=0
    while IFS= read -r playlist; do
        # Update extensions in playlist files
        if sed -i.bak \
            -e 's/\.m4a$/.opus/g' \
            -e 's/\.mp3$/.opus/g' \
            -e 's/\.webm$/.opus/g' \
            "$playlist" 2>/dev/null; then
            rm -f "$playlist.bak"
            playlist_count=$((playlist_count + 1))
        fi
    done < <(find "$PLAYLIST_DIR" -name "*.m3u" -type f)

    log_success "Updated $playlist_count playlist(s)"
}

parse_config() {
    local config_file="$1"

    if [ ! -f "$config_file" ]; then
        log_error "Config file not found: $config_file"
        exit 1
    fi

    log_info "Parsing config file: $config_file"

    # Use Python to parse YAML and extract artist/show paths
    python3 - "$config_file" <<'EOF'
import sys
import yaml

config_file = sys.argv[1]

with open(config_file, 'r') as f:
    config = yaml.safe_load(f)

# Extract paths from mixcloud shows
for show in config.get('mixcloud_shows', []):
    artist = show.get('artist', '').strip()
    show_name = show.get('show', '').strip()
    if artist and show_name:
        print(f"{artist}/{show_name}")

# Extract paths from soundcloud shows
for show in config.get('soundcloud_shows', []):
    artist = show.get('artist', '').strip()
    show_name = show.get('show', '').strip()
    if artist and show_name:
        print(f"{artist}/{show_name}")
EOF
}

scan_files_from_config() {
    local config_file="$1"

    log_info "Scanning for audio files from config: $config_file"

    if [ ! -d "$LIBRARY_DIR" ]; then
        log_error "Library directory not found: $LIBRARY_DIR"
        exit 1
    fi

    # Get show paths from config
    local show_paths=()
    while IFS= read -r path; do
        # Skip empty paths
        if [ -n "$path" ]; then
            show_paths+=("$path")
        fi
    done < <(parse_config "$config_file")

    if [ ${#show_paths[@]} -eq 0 ]; then
        log_error "No shows found in config file"
        exit 1
    fi

    log_info "Found ${#show_paths[@]} show(s) in config:"
    for path in "${show_paths[@]}"; do
        log_info "  - $path"
    done
    echo ""

    # Find audio files only in configured show directories
    for show_path in "${show_paths[@]}"; do
        # Skip empty paths (defensive check)
        if [ -z "$show_path" ]; then
            continue
        fi
        local show_dir="$LIBRARY_DIR/$show_path"
        if [ -d "$show_dir" ]; then
            find "$show_dir" -type f \( \
                -name "*.m4a" -o \
                -name "*.mp3" -o \
                -name "*.webm" \
            \)
        else
            log_warn "Show directory not found: $show_dir"
        fi
    done | sort
}

scan_files() {
    if [ "$CONFIG_ONLY" = "true" ]; then
        if [ -z "$CONFIG_FILE" ]; then
            log_error "CONFIG_ONLY=true requires --config option"
            exit 1
        fi
        scan_files_from_config "$CONFIG_FILE"
    else
        log_info "Scanning for audio files in: $LIBRARY_DIR"

        if [ ! -d "$LIBRARY_DIR" ]; then
            log_error "Library directory not found: $LIBRARY_DIR"
            exit 1
        fi

        # Find all audio files that aren't already opus
        find "$LIBRARY_DIR" -type f \( \
            -name "*.m4a" -o \
            -name "*.mp3" -o \
            -name "*.webm" \
        \) | sort
    fi
}

main() {
    echo "╔════════════════════════════════════════════════════════════╗"
    echo "║        Music Library Opus Conversion Tool                 ║"
    echo "╚════════════════════════════════════════════════════════════╝"
    echo ""

    log_info "Music directory: $MUSIC_DIR"
    log_info "Library directory: $LIBRARY_DIR"
    log_info "Opus bitrate: $OPUS_BITRATE"
    log_info "Parallel jobs: $PARALLEL_JOBS"

    if [ -n "$CONFIG_FILE" ]; then
        log_info "Config file: $CONFIG_FILE"
    fi

    if [ "$CONFIG_ONLY" = "true" ]; then
        log_info "Mode: Convert only configured shows from config file"
    else
        log_info "Mode: Convert entire library"
    fi

    if [ "$DRY_RUN" = "true" ]; then
        log_warn "DRY-RUN MODE: No files will be modified"
    fi

    echo ""

    check_dependencies

    # Scan for files
    mapfile -t all_files < <(scan_files)

    # Filter out empty entries
    files=()
    for file in "${all_files[@]}"; do
        if [ -n "$file" ]; then
            files+=("$file")
        fi
    done

    TOTAL_FILES=${#files[@]}

    if [ "$TOTAL_FILES" -eq 0 ]; then
        log_info "No files to convert!"
        exit 0
    fi

    log_info "Found $TOTAL_FILES file(s) to convert"
    echo ""

    # Confirm before proceeding (unless dry-run)
    if [ "$DRY_RUN" != "true" ]; then
        read -p "Proceed with conversion? [y/N] " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            log_info "Cancelled by user"
            exit 0
        fi
        echo ""
    fi

    # Convert files
    log_info "Starting conversion..."
    echo ""

    # Export function and variables for parallel execution
    export -f convert_file log_info log_success log_warn log_error human_readable_size
    export OPUS_BITRATE DRY_RUN RED GREEN YELLOW BLUE NC

    # Process files in parallel
    printf '%s\0' "${files[@]}" | xargs -0 -P "$PARALLEL_JOBS" -I {} bash -c 'convert_file "$@"' _ {}

    echo ""
    log_info "Conversion complete!"
    echo ""

    # Update playlists
    update_playlists

    # Print statistics
    echo "╔════════════════════════════════════════════════════════════╗"
    echo "║                    Conversion Summary                      ║"
    echo "╚════════════════════════════════════════════════════════════╝"
    echo ""
    echo "Total files found:    $TOTAL_FILES"
    echo "Successfully converted: $CONVERTED"
    echo "Skipped (exists):     $SKIPPED"
    echo "Failed:               $FAILED"

    if [ "$DRY_RUN" != "true" ] && [ "$SPACE_SAVED" -gt 0 ]; then
        echo "Space saved:          $(human_readable_size "$SPACE_SAVED")"
    fi

    echo ""

    if [ "$FAILED" -gt 0 ]; then
        log_warn "Some files failed to convert. Check the output above for details."
        exit 1
    fi

    if [ "$DRY_RUN" = "true" ]; then
        log_info "Dry-run complete. Run without DRY_RUN=true to perform actual conversion."
    else
        log_success "All files converted successfully!"
    fi
}

# Handle script arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        --music-dir)
            MUSIC_DIR="$2"
            LIBRARY_DIR="${MUSIC_DIR}/library"
            PLAYLIST_DIR="${MUSIC_DIR}/playlist"
            shift 2
            ;;
        --bitrate)
            OPUS_BITRATE="$2"
            shift 2
            ;;
        --jobs|-j)
            PARALLEL_JOBS="$2"
            shift 2
            ;;
        --config|-c)
            CONFIG_FILE="$2"
            CONFIG_ONLY=true
            shift 2
            ;;
        --all)
            CONFIG_ONLY=false
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Convert music library to Opus format"
            echo ""
            echo "Options:"
            echo "  --dry-run              Show what would be converted without doing it"
            echo "  --music-dir DIR        Music directory (default: /neo/music)"
            echo "  --config, -c FILE      Only convert shows from config file"
            echo "  --all                  Convert entire library (default if no --config)"
            echo "  --bitrate RATE         Opus bitrate (default: 128k)"
            echo "  --jobs, -j N           Number of parallel jobs (default: 4)"
            echo "  --help, -h             Show this help message"
            echo ""
            echo "Environment variables:"
            echo "  MUSIC_DIR              Same as --music-dir"
            echo "  CONFIG_FILE            Path to music-playlist-dl.yaml"
            echo "  OPUS_BITRATE           Same as --bitrate"
            echo "  PARALLEL_JOBS          Same as --jobs"
            echo "  DRY_RUN                Set to 'true' for dry-run mode"
            echo "  CONFIG_ONLY            Set to 'true' to convert only configured shows"
            echo ""
            echo "Examples:"
            echo "  # Preview conversion of entire library"
            echo "  $0 --dry-run"
            echo ""
            echo "  # Convert only podcast shows from config"
            echo "  $0 --config /neo/music/music-playlist-dl.yaml"
            echo ""
            echo "  # Convert entire library with higher quality"
            echo "  $0 --all --bitrate 192k --jobs 8"
            echo ""
            echo "  # Use custom directory and config"
            echo "  $0 --music-dir /mnt/music --config /mnt/music/config.yaml"
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

main
