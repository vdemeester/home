# Music Playlist Downloader

Automated downloader for electronic music podcasts and radio shows from Mixcloud and SoundCloud with automatic M3U playlist generation.

## Overview

This tool downloads episodic DJ podcasts/radio shows and organizes them by Artist/Show name for better library management. Files are tagged with proper metadata (artist, album) for music player compatibility, and M3U playlists are automatically generated for each show.

## Features

- **Automated Downloads**: Download from Mixcloud and SoundCloud
- **Organized Storage**: Files organized as `library/{artist}/{show}/`
- **Playlist Generation**: Automatic M3U playlists in `playlist/` directory
- **Metadata Support**: Proper artist and album tags
- **Resume Support**: Continue interrupted downloads
- **Notification Support**: ntfy notifications on success/failure
- **NixOS Integration**: Systemd timer for scheduled execution

## Configuration

### YAML Config File

Copy `config.yaml.example` to `/neo/music/music-playlist-dl.yaml` and customize:

```yaml
base_dir: /neo/music

mixcloud_shows:
  - handle: aboveandbeyond
    artist: Above & Beyond
    show: Group Therapy

soundcloud_shows:
  - url: https://soundcloud.com/clublifebytiesto
    artist: Tiësto
    show: CLUBLIFE

yt_dlp_options:
  format: best
  add_metadata: true
  embed_thumbnail: true
  continue: true
  ignore_errors: true
```

### NixOS Module

Enable in your NixOS configuration:

```nix
services.music-playlist-dl = {
  enable = true;
  user = "vincent";
  configFile = "/neo/music/music-playlist-dl.yaml";
  baseDir = "/neo/music";
  interval = "weekly"; # hourly, daily, weekly, or monthly
  onCalendar = "Sun *-*-* 02:00:00"; # Custom schedule (overrides interval)
  notification = {
    enable = true;
    ntfyUrl = "https://ntfy.sbr.pm";
    topic = "homelab";
  };
};
```

## Directory Structure

After running, your directory structure will look like:

```
/neo/music/
├── library/
│   ├── Above & Beyond/
│   │   └── Group Therapy/
│   │       ├── Group Therapy 657-abc123.m4a
│   │       └── Group Therapy 658-def456.m4a
│   ├── Armin van Buuren/
│   │   └── A State of Trance/
│   │       └── ASOT Episode 1255-xyz789.m4a
│   └── Tiësto/
│       └── CLUBLIFE/
│           └── CLUBLIFE Podcast 908-ghi012.m4a
└── playlist/
    ├── Above & Beyond - Group Therapy.m3u
    ├── Armin van Buuren - A State of Trance.m3u
    └── Tiësto - CLUBLIFE.m3u
```

## Playlist Format

Playlists are standard M3U format with relative paths from the playlist directory:

```m3u
#EXTM3U
../library/Above & Beyond/Group Therapy/Group Therapy 657-abc123.m4a
../library/Above & Beyond/Group Therapy/Group Therapy 658-def456.m4a
```

This allows music players to correctly resolve the file paths regardless of where they're accessed from.

## Usage

### Manual Execution

```bash
# Run with default config
music-playlist-dl

# Run with custom config
music-playlist-dl --config /path/to/config.yaml

# Verbose output
music-playlist-dl --verbose
```

### Systemd Service

```bash
# Start download manually
systemctl start music-playlist-dl

# Check status
systemctl status music-playlist-dl

# View logs
journalctl -u music-playlist-dl

# Check timer status
systemctl list-timers music-playlist-dl
```

## Podcast Sources

See `config.yaml.example` for a comprehensive list of supported podcasts with links to official sources.

Popular shows include:
- **Above & Beyond - Group Therapy**: Weekly trance podcast (ABGT)
- **Armin van Buuren - A State of Trance**: Longest-running trance show (since 2001)
- **Cosmic Gate - Wake Your Mind Radio**: Weekly progressive/trance show
- **Ferry Corsten - Resonation Radio**: Weekly electronic music show
- **Paul van Dyk - VONYC Sessions**: Grammy winner's weekly show
- **Tiësto - CLUBLIFE**: Weekly club tracks since 2007

## Requirements

- Python 3
- yt-dlp
- PyYAML

All dependencies are automatically handled by the Nix package.

## Converting Existing Files to Opus

If you already have downloads in other formats (M4A, MP3, WebM) and want to switch to Opus without re-downloading, use the conversion script.

**No installation required!** The script uses `nix-shell` to automatically provide all dependencies (ffmpeg with opus support and Python YAML parser).

### Convert Only Podcast Downloads (Recommended)

Convert only the shows configured in your `music-playlist-dl.yaml`:

```bash
# Preview what would be converted from config
./tools/music-playlist-dl/convert-to-opus.sh \
    --config /net/rhea/music/music-playlist-dl.yaml \
    --dry-run

# Convert only configured podcast shows
./tools/music-playlist-dl/convert-to-opus.sh \
    --config /net/rhea/music/music-playlist-dl.yaml \
    --jobs 8
```

This will:
- Read your config file to find all configured shows (Above & Beyond, Armin van Buuren, etc.)
- Convert **only** files in those show directories
- Leave the rest of your music library untouched

### Convert Entire Music Library

Convert everything in your library directory:

```bash
# Preview entire library conversion
./tools/music-playlist-dl/convert-to-opus.sh --all --dry-run

# Convert entire library with higher quality
./tools/music-playlist-dl/convert-to-opus.sh --all --bitrate 192k --jobs 8
```

### What the Script Does

- Automatically fetches ffmpeg with opus support via Nix
- Parses YAML config to identify podcast directories (with `--config`)
- Finds M4A, MP3, and WebM files in target directories
- Converts to Opus format preserving metadata and artwork
- Removes original files after successful conversion
- Updates M3U playlists to reference new .opus files
- Shows space savings and statistics
- Supports parallel processing for faster conversion

### Options

- `--config FILE` / `-c FILE` - Only convert shows from config file
- `--all` - Convert entire library (default if no `--config`)
- `--dry-run` - Preview without making changes
- `--bitrate RATE` - Opus bitrate (default: 128k)
- `--jobs N` / `-j N` - Number of parallel conversion jobs (default: 4)
- `--music-dir DIR` - Music directory (default: /neo/music)

### Performance Notes

**Config mode** (4 podcast shows, ~2,000 files):
- Conversion time: ~30-60 minutes (8 parallel jobs)
- Space savings: 30-40% reduction

**Full library** (~8,000 files):
- Conversion time: 2-4 hours (8 parallel jobs)
- Space savings: Varies by source format

**First run:** The script will download dependencies via Nix (one-time setup, ~30 seconds). Subsequent runs start immediately.

## Migration from Old Script

If you have files in the old structure (directly under artist name instead of `library/artist/show/`):

```bash
# Example: Move Above & Beyond files
mkdir -p "/neo/music/library/Above & Beyond/Group Therapy"
mv /neo/music/mixes/"Above & Beyond"/*.m4a "/neo/music/library/Above & Beyond/Group Therapy/" 2>/dev/null || true
```

## Notes

- Downloads continue from where they left off (uses `-c` flag)
- Failed downloads for individual shows don't stop the entire script
- Playlists are regenerated on each run to include new episodes
- The tool preserves existing files and only downloads new content

## License

MIT
