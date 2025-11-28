# download-kiwix-zim

Interactive tool to browse and download ZIM files from the Kiwix library.

## Features

- Browse **all 3,395 offline content archives** from the Kiwix catalog
- Filter by language (e.g., English, French, Spanish)
- Interactive selection with fzf (multi-select support)
- Fast parallel downloads with aria2c (4 connections)
- Automatic fallback to wget if aria2c is unavailable
- Human-readable file sizes (GB/MB)
- Preview mode showing language, size, and content type

## Installation

The package is available in this Nix flake:

```bash
# Build locally
nix build .#download-kiwix-zim

# Run directly
nix run .#download-kiwix-zim

# Install to profile
nix profile install .#download-kiwix-zim
```

## Usage

```bash
# Browse all ZIM files
download-kiwix-zim

# Filter by language code
download-kiwix-zim --lang eng

# Download to specific directory
download-kiwix-zim /mnt/gaia/kiwix

# Combine options
download-kiwix-zim --lang fra /mnt/gaia/kiwix
```

## fzf Keybindings

- **Tab**: Select/deselect item
- **Ctrl-A**: Select all
- **Ctrl-D**: Deselect all
- **Enter**: Confirm and download
- **Esc**: Cancel

## Language Codes

Common language codes:
- `eng` - English
- `fra` - French
- `spa` - Spanish
- `deu` - German
- `por` - Portuguese
- `ara` - Arabic
- `zho` - Chinese
- `jpn` - Japanese

## Example Output

```
Fetching Kiwix catalog...
Parsing catalog...
Found 3395 ZIM files

Select ZIM file to download (Tab for multi-select, Enter to confirm)

> Wikipedia [eng] 95.2G maxi
  Wikivoyage [eng] 156M nopic
  Wiktionary [eng] 4.2G maxi
  Stack Exchange [eng] 23.1G all
```

## Dependencies

All dependencies are automatically included in the Nix package:
- Python 3
- fzf (interactive selection)
- aria2c (parallel downloads)
- wget (fallback downloader)

## API

Uses the Kiwix OPDS v2 catalog API:
- Endpoint: `https://library.kiwix.org/catalog/v2/entries`
- Format: OPDS Atom XML feed
- Documentation: https://wiki.kiwix.org/wiki/OPDS
