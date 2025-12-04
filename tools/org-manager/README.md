# org-manager

A comprehensive tool for managing org-mode files with features for backup, validation, and link checking.

## Features

- **Backup**: Backup readwise and pkai notes to a specified destination
- **Validation**: Validate org-mode file structure and metadata
- **Link Checking**: Check for broken or invalid links (local, denote, id links)
- **Readwise Integration**: Wrapper for go-org-readwise to sync highlights

## Installation

Build and install using Nix:

```bash
nix build .#org-manager
# Or install to your profile
nix profile install .#org-manager
```

## Usage

### Global Flags

- `--org-dir <path>`: Path to org directory (default: `~/desktop/org`)
- `--version`: Show version information

### Commands

#### backup

Backup readwise or pkai notes to a destination directory.

```bash
# Backup all readwise notes
org-manager backup --type readwise --dest ~/backups

# Backup pkai notes without timestamp
org-manager backup --type pkai --dest ~/backups/pkai --timestamp=false

# Backup all notes (both readwise and pkai)
org-manager backup --type all --dest ~/backups
```

**Flags:**
- `--type <readwise|pkai|all>`: Type of notes to backup (default: all)
- `--dest <path>`: Destination directory (required)
- `--timestamp`: Add timestamp to backup directory name (default: true)
- `--compress`: Create compressed tar.gz archive (not yet implemented)

#### validate

Validate org-mode files for correctness.

```bash
# Validate all org files
org-manager validate

# Validate with verbose output
org-manager validate --verbose

# Only check metadata (skip structure checks)
org-manager validate --check-structure=false
```

**Flags:**
- `--check-all`: Check all org files in directory (default: true)
- `--check-metadata`: Validate org-mode metadata (default: true)
- `--check-structure`: Validate org-mode structure (default: true)
- `--verbose`, `-v`: Show verbose output

**Checks performed:**
- Required metadata (#+title, #+identifier)
- Proper headline structure (no level jumps)
- Balanced brackets and parentheses
- Empty metadata values

#### check-links

Check for broken or invalid links in org files.

```bash
# Check links with default text output
org-manager check-links

# Output results as JSON
org-manager check-links --format json

# Verbose output showing all checked links
org-manager check-links --verbose
```

**Flags:**
- `--format <text|json>`: Output format (default: text)
- `--verbose`, `-v`: Show verbose output including valid links

**Link types checked:**
- Local file links (`file:...`)
- Denote links (`denote:IDENTIFIER`)
- ID links (`id:...`)
- HTTP/HTTPS links (basic validation)

#### readwise

Wrapper for go-org-readwise to sync highlights.

```bash
# Sync readwise highlights
org-manager readwise --sync

# Dry run to see what would be synced
org-manager readwise --sync --dry-run

# Verbose output
org-manager readwise --sync --verbose
```

**Flags:**
- `--sync`: Sync readwise highlights (required)
- `--dry-run`: Show what would be done without making changes
- `--verbose`, `-v`: Show verbose output

**Note:** This command requires `go-org-readwise` to be installed and available in PATH.

## Project Structure

This project follows the standard Go project layout:

```
org-manager/
├── cmd/
│   └── org-manager/      # Main application entry point
│       └── main.go
├── internal/             # Private application code
│   ├── backup/          # Backup functionality
│   │   ├── backup.go
│   │   └── backup_test.go
│   ├── validate/        # Validation logic
│   │   ├── validate.go
│   │   └── validate_test.go
│   ├── links/           # Link checking
│   │   └── links.go
│   └── readwise/        # Readwise integration
│       └── readwise.go
├── go.mod               # Go module definition
├── go.sum               # Dependency checksums
├── default.nix          # Nix package definition
└── README.md
```

The tool expects org files to be located in `<org-dir>/notes/` by default.

### Supported Note Types

- **Readwise notes**: Files containing `==readwise=` in the filename
- **PKAI notes**: Files containing `==pkai--` in the filename

### Required Metadata

All org files should include:
- `#+title:` - The note title
- `#+identifier:` - A unique identifier (typically timestamp-based)

Example:
```org
#+title: My Note Title
#+identifier: 20231028T082123
#+filetags: :tag1:tag2:

* Content here
```

## Development

### Building from Source

```bash
cd tools/org-manager
go build
```

### Testing

```bash
go test ./...
```

## Future Enhancements

- Compressed backup support (tar.gz)
- Additional validation checks
- Link repair functionality
- Statistics and reporting
- Integration with other org-mode tools
