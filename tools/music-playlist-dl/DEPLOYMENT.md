# Deployment Guide for music-playlist-dl

## What Was Implemented

### 1. NixOS Module (`modules/music-playlist-dl.nix`)
- Full systemd service and timer integration
- Configurable schedule (hourly, daily, weekly, monthly, or custom)
- ntfy notification support (success and failure)
- Security hardening with systemd sandboxing
- Automatic directory creation

### 2. Python Downloader (`tools/music-playlist-dl/`)
- Downloads from Mixcloud and SoundCloud
- YAML-based configuration
- Automatic M3U playlist generation
- Proper metadata handling (artist, album)
- Error handling and logging
- Located in `tools/` for consistency with other custom tools

### 3. Integration in rhea
- Module imported in `systems/rhea/extra.nix`
- Service configured to run weekly on Sundays at 2 AM
- Notifications enabled via ntfy

## Deployment Steps

### Step 1: Create Configuration File

Copy the example config to the target location:

```bash
ssh rhea "mkdir -p /neo/music/library /neo/music/playlist"
```

Create `/neo/music/music-playlist-dl.yaml` on rhea with your desired shows:

```yaml
base_dir: /neo/music

mixcloud_shows:
  - handle: aboveandbeyond
    artist: Above & Beyond
    show: Group Therapy
  - handle: ArminvanBuuren
    artist: Armin van Buuren
    show: A State of Trance
  # Add more shows as needed

soundcloud_shows:
  - url: https://soundcloud.com/clublifebytiesto
    artist: Tiësto
    show: CLUBLIFE
  # Add more shows as needed

yt_dlp_options:
  format: best
  add_metadata: true
  embed_thumbnail: true
  continue: true
  ignore_errors: true
```

### Step 2: Build and Deploy

From your home repository:

```bash
# Test build locally first
make host/rhea/build

# If successful, deploy to rhea
make host/rhea/switch
```

### Step 3: Verify Deployment

After deployment, verify the service is configured correctly:

```bash
ssh rhea

# Check if service is installed
systemctl status music-playlist-dl

# Check timer status
systemctl list-timers music-playlist-dl

# View service configuration
systemctl cat music-playlist-dl

# Test manual run (optional)
sudo systemctl start music-playlist-dl

# Watch logs during test run
journalctl -u music-playlist-dl -f
```

### Step 4: Initial Run

For the first run, you may want to run it manually to ensure everything works:

```bash
ssh rhea
sudo systemctl start music-playlist-dl
journalctl -u music-playlist-dl -f
```

Expected behavior:
1. Creates `/neo/music/library/` and `/neo/music/playlist/` directories
2. Downloads episodes from configured shows
3. Generates M3U playlists
4. Sends ntfy notification on completion

## Configuration Options

### Service Configuration (in `systems/rhea/extra.nix`)

```nix
services.music-playlist-dl = {
  enable = true;               # Enable/disable service
  user = "vincent";            # User to run as
  configFile = "/neo/music/music-playlist-dl.yaml";  # Config file path
  baseDir = "/neo/music";      # Base directory for downloads

  # Schedule options (pick one):
  interval = "weekly";         # Predefined: hourly, daily, weekly, monthly
  # OR
  onCalendar = "Sun *-*-* 02:00:00";  # Custom systemd timer format

  notification = {
    enable = true;             # Enable ntfy notifications
    ntfyUrl = "https://ntfy.sbr.pm";
    topic = "homelab";
  };
};
```

### YAML Configuration

- `base_dir`: Where to store downloads and playlists
- `mixcloud_shows`: List of Mixcloud shows to download
- `soundcloud_shows`: List of SoundCloud shows/playlists to download
- `yt_dlp_options`: Options passed to yt-dlp

## Directory Structure After Deployment

```
/neo/music/
├── music-playlist-dl.yaml          # Configuration file
├── library/                        # Downloaded audio files
│   ├── Above & Beyond/
│   │   └── Group Therapy/
│   │       ├── Group Therapy 657-abc123.m4a
│   │       └── Group Therapy 658-def456.m4a
│   └── Armin van Buuren/
│       └── A State of Trance/
│           └── ASOT Episode 1255-xyz789.m4a
└── playlist/                       # Generated M3U playlists
    ├── Above & Beyond - Group Therapy.m3u
    └── Armin van Buuren - A State of Trance.m3u
```

## Systemd Commands

```bash
# Start download manually
sudo systemctl start music-playlist-dl

# Check service status
systemctl status music-playlist-dl

# View recent logs
journalctl -u music-playlist-dl -n 50

# Follow logs in real-time
journalctl -u music-playlist-dl -f

# Check timer schedule
systemctl list-timers music-playlist-dl

# Disable automatic runs
sudo systemctl stop music-playlist-dl.timer
sudo systemctl disable music-playlist-dl.timer

# Re-enable automatic runs
sudo systemctl enable music-playlist-dl.timer
sudo systemctl start music-playlist-dl.timer
```

## Troubleshooting

### Downloads fail
- Check network connectivity from rhea
- Verify yt-dlp can access Mixcloud/SoundCloud
- Check logs: `journalctl -u music-playlist-dl`

### Playlists not generated
- Verify `/neo/music/playlist/` directory exists and is writable
- Check if audio files were downloaded successfully
- Look for errors in logs

### No ntfy notifications
- Verify ntfy server is accessible from rhea
- Check notification configuration in module
- Test manually: `curl -d "test" https://ntfy.sbr.pm/homelab`

### Permission issues
- Ensure user 'vincent' has write access to `/neo/music/`
- Check service user in systemd configuration
- Verify directory ownership: `ls -la /neo/music/`

## Migration from Old Script

If you have existing files in `/net/rhea/music/mixes/`:

```bash
# Move to new structure
ssh rhea
cd /neo/music

# For each artist, create the new structure
# Example for Above & Beyond:
mkdir -p "library/Above & Beyond/Group Therapy"
mv "/net/rhea/music/mixes/Above & Beyond"/*.m4a "library/Above & Beyond/Group Therapy/" || true

# After migration, run the downloader to generate playlists
sudo systemctl start music-playlist-dl
```

## Next Steps

1. **Test build**: `make host/rhea/build`
2. **Create config file** on rhea at `/neo/music/music-playlist-dl.yaml`
3. **Deploy**: `make host/rhea/switch` (requires user confirmation)
4. **Manual test**: `ssh rhea sudo systemctl start music-playlist-dl`
5. **Verify playlists**: Check `/neo/music/playlist/`
6. **Monitor first scheduled run**: Check logs after Sunday 2 AM

## Deadline

This implementation addresses the TODO with deadline **2025-12-19 Fri** (in 3 days).

All components are ready for deployment. The service will run weekly on Sundays at 2 AM and send notifications to the homelab ntfy topic.
