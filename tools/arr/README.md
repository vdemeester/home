# arr - Unified CLI for *arr Services

A command-line tool for managing Sonarr, Radarr, Lidarr, and Jellyfin media servers.

## Features

- **Sonarr**: Rename TV series episodes
- **Radarr**: Rename movies
- **Lidarr**:
  - Rename albums
  - Retag album metadata
  - Update library paths
  - **Sync Spotify playlists** - automatically add artists from your Spotify playlists
- **Jellyfin**:
  - **Sync Spotify playlists to Jellyfin** - create playlists in Jellyfin from your Spotify playlists

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

### Shell Completion

Shell completions are automatically installed when using the Nix package. If you need to manually generate completion scripts:

```bash
# Bash
arr completion bash > ~/.local/share/bash-completion/completions/arr
# Or system-wide: arr completion bash | sudo tee /etc/bash_completion.d/arr

# Zsh
arr completion zsh > ~/.zsh/completions/_arr
# Then add to .zshrc: fpath=(~/.zsh/completions $fpath)

# Fish
arr completion fish > ~/.config/fish/completions/arr.fish
```

After installing, restart your shell or source the completion file.

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

## Jellyfin Playlist Sync

Sync your Spotify playlists to Jellyfin by matching tracks in your Jellyfin library.

### Prerequisites

1. **Get Jellyfin API Token**:
   - Log in to Jellyfin web interface
   - Go to Dashboard → API Keys
   - Click "+" to create a new API key
   - Give it a name (e.g., "arr script")
   - Copy the generated API token

2. **Get Jellyfin User ID**:
   - Go to Dashboard → Users
   - Click on your username
   - Look at the URL: `http://your-jellyfin/web/index.html#!/users/user.html?userId=USER_ID_HERE`
   - Copy the User ID from the URL

3. **Set up Spotify credentials** (same as Lidarr sync):
   - Follow the Spotify setup instructions in the "Spotify Playlist Sync" section above
   - You need: Client ID, Client Secret, and optionally your Spotify username

### Quick Start

```bash
# Set environment variables
export JELLYFIN_URL="http://localhost:8096"  # Optional, defaults to http://localhost:8096
export JELLYFIN_API_TOKEN="your-jellyfin-api-token"
export JELLYFIN_USER_ID="your-jellyfin-user-id"
export SPOTIFY_CLIENT_ID="your-spotify-client-id"
export SPOTIFY_CLIENT_SECRET="your-spotify-client-secret"
export SPOTIFY_USERNAME="your-spotify-username"

# Interactive mode - select playlists with fzf (uses env vars)
arr jellyfin sync-spotify

# Or with flags
arr jellyfin sync-spotify \
    --url http://localhost:8096 \
    -t your-token \
    -i your-user-id \
    -u your-username
```

### Usage

The tool supports three modes of operation:

| Mode | Command | Description |
|------|---------|-------------|
| **Interactive (fzf)** | `arr jellyfin sync-spotify -u USERNAME` | Select playlists interactively with fzf |
| **All playlists** | `arr jellyfin sync-spotify -u USERNAME --all` | Sync all public playlists from user |
| **Specific playlists** | `arr jellyfin sync-spotify -p ID1 -p ID2` | Sync specific playlists by ID |

#### Mode 1: Interactive Selector with fzf (Recommended)

Select playlists interactively using fzf. Requires `--spotify-username`.

```bash
# With environment variables (URL defaults to http://localhost:8096)
arr jellyfin sync-spotify

# Or with flags
arr jellyfin sync-spotify -u your-spotify-username

# With dry run
arr jellyfin sync-spotify -u your-spotify-username --dry-run
```

**Interactive controls:**
- `TAB`: Select/deselect a playlist
- `↑/↓` or `j/k`: Navigate
- `ENTER`: Confirm selection
- `ESC` or `Ctrl+C`: Cancel
- Type to filter playlists

#### Mode 2: Sync ALL User Playlists

Sync all public playlists from a Spotify user. Requires `--spotify-username` and `--all`.

```bash
# Sync all playlists (no fzf selection)
arr jellyfin sync-spotify -u your-spotify-username --all

# With dry run to preview
arr jellyfin sync-spotify -u your-spotify-username --all --dry-run
```

#### Mode 3: Sync Specific Playlists by ID

Sync specific playlists using `-p` flag (can be repeated).

```bash
# Sync specific playlists
arr jellyfin sync-spotify \
    -p PLAYLIST_ID_1 -p PLAYLIST_ID_2

# With all options
arr jellyfin sync-spotify \
    --url http://localhost:8096 \
    --api-token your-token \
    --user-id your-user-id \
    --spotify-client-id your-id \
    --spotify-client-secret your-secret \
    -p PLAYLIST_ID_1 -p PLAYLIST_ID_2
```

### Options

```bash
# Adjust match threshold (default: 0.6)
# Lower = more matches but more false positives
# Higher = fewer matches but more accurate
arr jellyfin sync-spotify --match-threshold 0.4

# Make playlists public (default: private)
arr jellyfin sync-spotify --public

# Skip confirmation prompts (for automation)
arr jellyfin sync-spotify --no-confirm

# Custom Jellyfin URL (or use JELLYFIN_URL env var)
arr jellyfin sync-spotify --url http://jellyfin.example.com:8096
```

### How It Works

1. Fetches tracks from specified Spotify playlist(s)
2. For each track, searches your Jellyfin library using track name, artist, and album
3. Uses fuzzy matching with configurable threshold (default: 0.6) to find best matches
4. Creates corresponding playlists in Jellyfin with matched tracks
5. Reports matching statistics and lists unmatched tracks

**Matching Algorithm:**
- Track name match: 40% weight
- Artist name match: 40% weight
- Album name match: 20% weight
- Total score must be ≥ threshold (default 0.6) to be considered a match

### Example Output

```
================================================================================
SYNCING SPOTIFY PLAYLISTS TO JELLYFIN
================================================================================


Playlist: Chill Vibes (by username, 50 tracks)
  Retrieved 50 tracks from Spotify
  Matching tracks in Jellyfin library...
    [1/50] Searching: Song Name - Artist Name
      ✓ Matched (confidence: 0.85)
    [2/50] Searching: Another Song - Another Artist
      ✗ No match (best score: 0.45, threshold: 0.60)
    ...

  Matched 45/50 tracks (90.0%)

  ✓ Created playlist in Jellyfin (ID: abc123)

================================================================================
FINAL SUMMARY
================================================================================

Total playlists processed: 1
  - Created: 1
  - Skipped: 0

Total tracks processed: 50
  - Matched: 45
  - Failed to match: 5
  - Match rate: 90.0%

================================================================================
FAILED MATCHES
================================================================================

The following 5 tracks could not be matched (threshold: 0.60):

  • Song Name - Artist Name (from 'Chill Vibes')
    Album: Album Name, Best score: 0.45
  ...

Tip: Lower --match-threshold if too many false negatives. Default is 0.6.

✓ Successfully created 1 playlist(s) in Jellyfin!
```

### Troubleshooting

**Playlists already exist**: The tool skips playlists that already exist in Jellyfin (matched by name). Delete the existing playlist in Jellyfin if you want to recreate it.

**Low match rate**: If many tracks aren't matching:
1. Check that the tracks actually exist in your Jellyfin library
2. Lower the `--match-threshold` (try 0.4 or 0.5)
3. Verify your music library has proper metadata (track names, artist names, album names)

**No matches at all**: Verify that:
- Your Jellyfin library is properly indexed
- You're using the correct user ID (must have access to the music library)
- The music library contains the artists/albums you're trying to match

## Other Commands

### Lidarr

#### Queue Management

Manage items in the Lidarr queue with interactive selection. This is especially useful for handling items that need manual import or have errors.

```bash
# View and manage all queue items interactively
arr lidarr manage-queue http://localhost:8686 your-api-key

# Show only items that need manual import
arr lidarr manage-queue http://localhost:8686 your-api-key --filter manual

# Show only items with warnings
arr lidarr manage-queue http://localhost:8686 your-api-key --filter warning

# Show only items with errors
arr lidarr manage-queue http://localhost:8686 your-api-key --filter error

# Show only completed items
arr lidarr manage-queue http://localhost:8686 your-api-key --filter completed

# Show completed items where import failed
arr lidarr manage-queue http://localhost:8686 your-api-key --filter completed --tracked-state importFailed

# Show only items with specific tracked state
arr lidarr manage-queue http://localhost:8686 your-api-key --tracked-state importing

# Remove items and add to blocklist
arr lidarr manage-queue http://localhost:8686 your-api-key --blocklist

# Keep items in download client when removing from queue
arr lidarr manage-queue http://localhost:8686 your-api-key --keep-in-client

# Dry run to preview without making changes
arr lidarr manage-queue http://localhost:8686 your-api-key --filter manual --dry-run
```

**Filter types:**
- `all`: Show all queue items (default)
- `manual`: Items that need manual import
- `warning`: Items with warnings
- `error`: Items with errors
- `completed`: Items that have completed downloading

**Tracked states** (use with `--tracked-state`):
- `importFailed`: Items where import has failed
- `imported`: Successfully imported items
- `importing`: Currently importing items
- `failedPending`: Failed items pending retry
- And other tracked download states from Lidarr API

You can combine `--filter` and `--tracked-state` to narrow down results (e.g., completed items where import failed).

**Removal options:**
- `--remove-from-client` (default): Remove the item from your download client
- `--keep-in-client`: Leave the item in your download client
- `--blocklist`: Add the release to blocklist to prevent re-downloading
- `--skip-redownload`: Don't automatically search for a replacement

**Interactive controls:**
- `TAB`: Select/deselect a queue item
- `↑/↓` or `j/k`: Navigate queue items
- `ENTER`: Confirm selection
- `Ctrl+/`: Toggle preview window (shows detailed information)
- `Ctrl+↑/↓`: Scroll preview window up/down (page at a time)
- `Ctrl+u/d`: Scroll preview window up/down (half page at a time)
- `ESC` or `Ctrl+C`: Cancel
- Type to filter items

**Preview window shows:**
- Full item details (artist, album, release date)
- Download information (status, protocol, client)
- File size and download progress
- Quality settings
- Output path
- Error and status messages
- Download ID and timestamps

#### Other Lidarr Commands

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
