# audible-converter

Download and convert Audible audiobooks to Audiobookshelf-compatible formats.

## Overview

`audible-converter` is a wrapper tool that combines [audible-cli](https://github.com/mkb79/audible-cli) and [aaxtomp3](https://github.com/KrumpetPirate/AAXtoMP3) to:

- Download audiobooks from your Audible library
- Convert AAX and AAXC files to M4B/MP3/M4A formats
- Preserve chapter markers and metadata
- Organize output for Audiobookshelf

## Features

- **Download from Audible**: Sync your entire library or download specific books
- **AAX & AAXC format support**: Converts both AAX (older) and AAXC (newer) formats
- **Multiple output formats**: M4B (recommended), MP3, or M4A
- **Chapter preservation**: Maintains chapter markers and navigation
- **Metadata embedding**: Preserves title, author, narrator, cover art
- **Batch processing**: Handle multiple books at once
- **Quality options**: Choose between best, high, or normal audio quality
- **Automatic voucher handling**: AAXC files are decrypted using voucher files downloaded alongside

## Installation

This package is available in the home repository flake:

```nix
# In your NixOS configuration
environment.systemPackages = with pkgs; [
  audible-converter
];
```

Or install directly:

```bash
nix profile install .#audible-converter
```

## First-Time Setup

Before using the tool, authenticate with Audible:

```bash
# Step 1: Authenticate with Audible
audible quickstart
```

This will:
1. Prompt for your Audible credentials
2. Store authentication tokens securely
3. Set up your default region (US, UK, etc.)

```bash
# Step 2: Verify activation bytes are available
audible activation-bytes
```

This command retrieves your 8-character activation bytes (e.g., `1a2b3c4d`) which are used to decrypt AAX files. The tool will automatically use these bytes when converting.

## Usage

### Commands

```bash
# Sync library (download and convert new books)
audible-converter sync

# Download entire library
audible-converter download-all

# Download specific book by ASIN
audible-converter download B01234567X

# Convert existing AAX file
audible-converter convert /path/to/book.aax

# List your Audible library
audible-converter list
```

### Options

```bash
-o, --output DIR     Output directory (default: $HOME/audiobooks)
-t, --temp DIR       Temporary download directory (default: /tmp/audible-download)
-q, --quality QUAL   Audio quality: best, high, normal (default: best)
-f, --format FMT     Output format: m4b, mp3, m4a (default: m4b)
-h, --help           Show help message
```

### Examples

```bash
# Download and convert all books to /mnt/audiobooks
audible-converter --output /mnt/audiobooks sync

# Download specific book in MP3 format
audible-converter --format mp3 download B01234567X

# Convert existing AAX file with custom output
audible-converter --output ~/audiobooks convert book.aax
```

## Environment Variables

Configure defaults using environment variables:

- `AUDIBLE_OUTPUT_DIR` - Output directory for converted books
- `AUDIBLE_TEMP_DIR` - Temporary directory for downloads (kept by default for reuse)
- `AUDIBLE_QUALITY` - Audio quality setting (best, high, normal)
- `AUDIBLE_FORMAT` - Output format (m4b, mp3, m4a)
- `AUDIBLE_CLEANUP_ON_EXIT` - Set to `true` to auto-delete temp files on exit (default: false)

Example:

```bash
export AUDIBLE_OUTPUT_DIR="$HOME/audiobooks"
export AUDIBLE_FORMAT="m4b"
audible-converter sync
```

On rhea (with NFS-mounted storage):

```bash
export AUDIBLE_OUTPUT_DIR="/neo/audiobooks"
export AUDIBLE_TEMP_DIR="/neo/audiobooks/zz_import"  # Persistent for reuse
audible-converter sync
```

**Note**: By default, downloaded AAX files in `AUDIBLE_TEMP_DIR` are **kept** between runs to save bandwidth and time. Only new books will be downloaded on subsequent syncs. Set `AUDIBLE_CLEANUP_ON_EXIT=true` if you want to auto-delete temp files after each run.

## Output Structure

Books are automatically organized for Audiobookshelf:

```
/neo/audiobooks/
├── Brandon Sanderson/
│   ├── Mistborn The Final Empire/
│   │   ├── cover.jpg
│   │   └── Mistborn The Final Empire.m4b
│   └── The Way of Kings/
│       ├── cover.jpg
│       └── The Way of Kings.m4b
└── Andy Weir/
    └── The Martian/
        ├── cover.jpg
        └── The Martian.m4b
```

Each book includes:
- Single M4B file with embedded chapters
- Cover art (embedded and separate file)
- Metadata (title, author, narrator, etc.)

## Audiobookshelf Integration

Point your Audiobookshelf library to the output directory:

1. Open Audiobookshelf web interface
2. Go to Settings → Libraries
3. Add new library with path: `/neo/audiobooks`
4. Scan library to import books

## Format Comparison

| Format | Size | Quality | Compatibility | Chapters | Recommended |
|--------|------|---------|---------------|----------|-------------|
| M4B    | Small | High | Good | Yes | ✅ Best choice |
| MP3    | Medium | Good | Excellent | Limited | For legacy devices |
| M4A    | Small | High | Good | Yes | Alternative to M4B |

**Recommendation**: Use M4B format for best balance of quality, size, and chapter support.

## Troubleshooting

### Authentication Issues

If you get authentication errors:

```bash
# Re-run authentication
audible quickstart

# Check stored credentials
audible library list
```

### Activation Bytes / Authcode Issues

The tool automatically retrieves activation bytes using `audible activation-bytes`. If this fails:

**Check activation bytes are available:**
```bash
audible activation-bytes
```

**If command returns empty or errors:**
1. Ensure you're authenticated: `audible quickstart`
2. Try re-authenticating if needed
3. Activation bytes are account-specific and permanent (you only need to get them once)

**Manual authcode override:**
```bash
# Option 1: Command line
audible-converter --authcode 1a2b3c4d convert file.aax

# Option 2: Environment variable
export AUDIBLE_AUTHCODE=1a2b3c4d
audible-converter convert file.aax
```

**Note**: Your activation bytes are an 8-character hexadecimal code (e.g., `1a2b3c4d`). They are tied to your Audible account and remain constant.

### Download Failures

Some books may fail to download for various reasons:

**Non-downloadable books**: Some items in your Audible library cannot be downloaded:
- Podcasts (excluded via `--ignore-podcasts` flag)
- Plus Catalog books that have been removed
- Region-restricted content
- Books returned or removed from your library
- Special content types (guided meditations, sleep sounds, etc.)

The tool will skip these automatically and continue processing. Example error:
```
error: The Manager's Path is not downloadable.
```

**Other download issues**:
- Check the ASIN is correct: `audible library list`
- Verify the book is in your library for your region
- Check network connectivity

### AAXC Format Support

**AAXC files (newer Audible format) are fully supported** by aaxtomp3!

**How it works:**
- AAXC is a newer encryption format introduced by Audible
- When downloading with `--aaxc` flag, audible-cli downloads both the `.aaxc` file and a `.voucher` file
- The voucher file contains the decryption key and IV (initialization vector)
- `aaxtomp3` reads the voucher file and uses the key/IV to decrypt the AAXC file
- Conversion proceeds normally after decryption

**Download strategy:**
- The script uses `--aax-fallback` flag: tries AAX first, falls back to AAXC if AAX is unavailable
- Older books: typically available as AAX
- Newer books: may only be available as AAXC
- Both formats convert successfully ✓

**Requirements:**
- Voucher file must be present alongside the AAXC file
- Naming convention: `bookname.aaxc` and `bookname.voucher`
- audible-cli downloads both automatically when using `--aaxc` or `--aax-fallback`

### Conversion Issues

**For AAX files:**
1. Ensure you have the required dependencies (included in the package)
2. Verify activation bytes are available: `audible activation-bytes`
3. Check the AAX file is not corrupted
4. Try downloading the book again

**For AAXC files:**
1. Ensure the `.voucher` file exists alongside the `.aaxc` file
2. Check voucher file contains valid JSON with `key` and `iv` fields
3. Verify the voucher file was downloaded at the same time as the AAXC file
4. If voucher is missing, re-download the book with `--aaxc` flag

### Permission Errors

Ensure you have write permissions to the output directory:

```bash
# Check permissions
ls -ld /neo/audiobooks

# Fix if needed
sudo chown -R $USER:users /neo/audiobooks
```

## Related Documentation

- [Audible CLI Documentation](https://audible-cli.readthedocs.io/)
- [aaxtomp3 GitHub](https://github.com/KrumpetPirate/AAXtoMP3)
- [Audiobookshelf Documentation](https://www.audiobookshelf.org/docs)

## Implementation Note

For detailed information about the conversion workflow and technical decisions, see:
- Note: `~/desktop/org/notes/20251213T095555--audible-to-audiobookshelf-conversion-guide__audible_audiobooks_audiobookshelf_conversion_homelab_nixos_reference.org`
