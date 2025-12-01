# arr - Unified CLI for *arr Services

A command-line tool for managing Sonarr, Radarr, and Lidarr media servers.

## Features

- **Sonarr**: Rename TV series episodes
- **Radarr**: Rename movies
- **Lidarr**:
  - Rename albums
  - Retag album metadata
  - Update library paths
  - **Sync Spotify playlists** - automatically add artists from your Spotify playlists

## Installation

This package is built with Nix. From the repository root:

```bash
nix build .#arr
./result/bin/arr --help
```

Or install to your profile:

```bash
nix profile install .#arr
```

## Spotify Playlist Sync

### Quick Start

1. **Create a Spotify App** to get API credentials:
   - Go to https://developer.spotify.com/dashboard
   - Log in with your Spotify account
   - Click "Create an App"
   - Fill in app name: "Lidarr Sync" (or any name)
   - Accept Terms of Service and click "Create"
   - Note your **Client ID** and **Client Secret** (click "Show Client Secret")
   - No redirect URI needed!

2. **Get your Spotify username** (for interactive mode):
   - Go to https://www.spotify.com/account/
   - Your username is shown under "Account overview"
   - Or use any Spotify profile URL (the alphanumeric string after `/user/`)

3. **Set environment variables**:
   ```bash
   export LIDARR_API_KEY="your-lidarr-api-key"
   export SPOTIFY_CLIENT_ID="your-spotify-client-id"
   export SPOTIFY_CLIENT_SECRET="your-spotify-client-secret"
   export SPOTIFY_USERNAME="your-spotify-username"
   ```

4. **Run interactive mode**:
   ```bash
   arr lidarr sync-spotify http://localhost:8686
   ```

### Prerequisites

1. **Create a Spotify App** to get API credentials:
   - Go to https://developer.spotify.com/dashboard
   - Log in with your Spotify account
   - Click "Create an App"
   - Fill in the app name (e.g., "Lidarr Sync")
   - Accept the Terms of Service and click "Create"
   - Note your **Client ID** and **Client Secret** (click "Show Client Secret")
   - **No redirect URI or OAuth setup needed** - uses simple client credentials flow

2. **Get your Spotify username** (for interactive mode):
   - Go to https://www.spotify.com/account/ and look for "Username"
   - Or open your profile in Spotify app → ... → Share → Copy profile link
   - Extract the username from the URL: `https://open.spotify.com/user/USERNAME`

3. **Install fzf** (for interactive playlist selection):
   - On NixOS: `nix-env -iA nixpkgs.fzf` or include it in your system config
   - On other systems: see https://github.com/junegunn/fzf

4. **Get Playlist IDs** (optional - only needed for non-interactive mode):
   - Open Spotify and navigate to your playlist
   - Click "..." → "Share" → "Copy link to playlist"
   - The ID is the alphanumeric string in the URL:
     `https://open.spotify.com/playlist/37i9dQZF1DXcBWIGoYBM5M`
     → Playlist ID is `37i9dQZF1DXcBWIGoYBM5M`

5. **Configure Lidarr**:
   - Get your Lidarr API key from Settings → General → Security
   - Note your Lidarr URL (e.g., `http://localhost:8686`)
   - Set up a root folder for music in Settings → Media Management

### Usage

#### Interactive Mode (Recommended)

The easiest way to use this tool is interactive mode. It will:
1. Fetch all your **public** playlists from Spotify
2. Show them in an interactive fzf menu
3. Let you select multiple playlists with TAB
4. Sync the selected playlists to Lidarr

**Note**: Only public playlists can be accessed. Private playlists won't appear in the list.

```bash
# With environment variables (recommended for frequent use)
export LIDARR_API_KEY="your-lidarr-api-key"
export SPOTIFY_CLIENT_ID="your-spotify-client-id"
export SPOTIFY_CLIENT_SECRET="your-spotify-client-secret"
export SPOTIFY_USERNAME="your-spotify-username"

arr lidarr sync-spotify http://localhost:8686

# Or with flags
arr lidarr sync-spotify http://localhost:8686 \
    --api-key your-key \
    --spotify-client-id your-id \
    --spotify-client-secret your-secret \
    --spotify-username your-username

# Short form
arr lidarr sync-spotify http://localhost:8686 -u your-username
```

**Interactive controls:**
- `TAB`: Select/deselect a playlist
- `↑/↓` or `j/k`: Navigate
- `ENTER`: Confirm selection
- `ESC` or `Ctrl+C`: Cancel
- Type to filter playlists

#### Direct Mode (By Playlist ID)

If you already know the playlist IDs, you can specify them directly:

```bash
# Sync a single playlist (with environment variables)
arr lidarr sync-spotify http://localhost:8686 PLAYLIST_ID

# Sync multiple playlists
arr lidarr sync-spotify http://localhost:8686 \
    PLAYLIST_ID_1 PLAYLIST_ID_2 PLAYLIST_ID_3

# Or with flags
arr lidarr sync-spotify http://localhost:8686 \
    --api-key your-key \
    --spotify-client-id your-id \
    --spotify-client-secret your-secret \
    PLAYLIST_ID_1 PLAYLIST_ID_2
```

#### Additional Options

All options work with environment variables or flags:

```bash
# Dry run to preview changes (works in both modes)
arr lidarr sync-spotify http://localhost:8686 --dry-run

# Custom root folder
arr lidarr sync-spotify http://localhost:8686 --root-folder /data/music

# Monitor only future albums (don't search for existing releases)
arr lidarr sync-spotify http://localhost:8686 --monitor future

# Skip confirmation prompts (for automation)
arr lidarr sync-spotify http://localhost:8686 --no-confirm

# Adjust request delay (default: 1.5s) for slower/faster Lidarr instances
arr lidarr sync-spotify http://localhost:8686 --request-delay 3.0

# Combine options
arr lidarr sync-spotify http://localhost:8686 \
    --root-folder /data/music \
    --monitor future \
    --request-delay 2.0 \
    --dry-run
```

### Performance & Rate Limiting

The tool implements several features to avoid overwhelming Lidarr:

- **Automatic retries**: Retries failed requests up to 3 times with exponential backoff
- **Timeout handling**: 30-second timeout per request with automatic retry
- **Request delays**: Configurable delay between artist additions (default: 1.5s)
- **Error handling**: Graceful handling of 400/503 errors with detailed error messages

If you experience timeout or rate limiting errors:
1. Increase the delay: `--request-delay 3.0` or higher
2. Check your Lidarr server performance (CPU/disk usage)
3. Reduce the number of playlists processed at once

### Monitoring Options

The `--monitor` flag controls which albums Lidarr will monitor for each artist:

- `all` (default): Monitor all albums
- `future`: Only monitor future releases
- `missing`: Only monitor missing albums
- `existing`: Only monitor existing albums in your library
- `none`: Don't monitor any albums

### How It Works

1. Connects to Spotify using client credentials (no browser authorization needed)
2. Fetches all tracks from the specified Spotify playlist(s)
3. Extracts unique artists from the playlist tracks
4. Checks which artists are already in your Lidarr library
5. Searches Lidarr's MusicBrainz database for missing artists
6. Adds new artists to Lidarr with your specified monitoring settings
7. Shows which albums from the playlist each artist has released

### Important Notes

- **Public Playlists Only**: The tool uses Spotify's client credentials flow, which can only access public playlists. Make sure your playlists are set to "Public" in Spotify if you want to use interactive mode.
- **No OAuth Required**: Unlike the OAuth flow, you don't need to set up redirect URIs or authorize in a browser. Just create a Spotify app and use the credentials.
- **Username Required for Interactive Mode**: To list your playlists, you need to provide your Spotify username. You can find it in your Spotify account settings.

### Example Output

```
================================================================================
FETCHING SPOTIFY PLAYLISTS
================================================================================

Playlist: Discover Weekly (by Spotify, 30 tracks)
  Retrieved 30 tracks


Found 25 unique artists across all playlists

================================================================================
CHECKING LIDARR
================================================================================
Fetching existing artists from Lidarr...
Found 150 artists already in Lidarr

================================================================================
SUMMARY
================================================================================

Already in Lidarr (15 items):
  - Artist 1
  - Artist 2
  ...

→ Artists to add: 10
  - New Artist 1
  - New Artist 2
  ...

Add 10 artists to Lidarr? (y/n): y

================================================================================
ADDING ARTISTS TO LIDARR
================================================================================

Searching for: New Artist 1
  Found: New Artist 1
  Albums in playlists: 3
    - Album Name 1
    - Album Name 2
    - Album Name 3
  ✓ Added successfully (ID: 123)

...

================================================================================
FINAL SUMMARY
================================================================================

Total playlists processed: 1
Total unique artists found: 25
Artists already in Lidarr: 15
Artists to add: 10
  - Successfully added: 10
  - Failed: 0

Monitoring mode: all
New artists will start searching for albums based on your Lidarr settings.
```

## Other Commands

### Lidarr

```bash
# Rename albums
arr lidarr rename-albums http://localhost:8686 your-api-key

# Retag album metadata
arr lidarr retag-albums http://localhost:8686 your-api-key

# Update library paths
arr lidarr update-paths http://localhost:8686 your-api-key /data/music
```

### Sonarr

```bash
# Rename TV series episodes
arr sonarr rename http://localhost:8989 your-api-key
```

### Radarr

```bash
# Rename movies
arr radarr rename http://localhost:7878 your-api-key
```

All commands support `--dry-run` and `--no-confirm` flags.

## Development

The tool is structured as:
- `arr` - Main CLI entry point
- `lib.py` - Shared library (API clients, formatting utilities)
- `commands/` - Individual command implementations

To add a new command:
1. Create a new file in `commands/`
2. Add the command to the appropriate group in `arr`
3. Update `default.nix` if new dependencies are needed
