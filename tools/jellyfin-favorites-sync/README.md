# Jellyfin Favorites Sync

Sync Jellyfin favorite movies and series to a remote host via rsync.

## Overview

This tool queries a Jellyfin server for favorited items (movies and TV series), expands series to individual episodes, discovers parent directories containing media files and metadata (subtitles, .nfo files, posters), and syncs them to a remote host using rsync in mirror mode.

## Features

- **Favorites Query**: Automatically discovers movies and series marked as favorite
- **Series Expansion**: Expands favorited TV series to include all episodes
- **Complete Sync**: Syncs parent directories to include all auxiliary files (.srt, .nfo, poster.jpg, etc.)
- **Mirror Mode**: Uses rsync `--delete` to remove files when items are unfavorited
- **Efficient**: Single rsync invocation using `--files-from` for large favorite sets
- **Resumable**: Supports `--partial` and `--append-verify` for interrupted transfers
- **Configurable**: Target host, paths, and SSH arguments all configurable

## Usage

### CLI

```bash
jellyfin-favorites-sync \
  --jellyfin-url https://jellyfin.sbr.pm \
  --api-key-file /run/agenix/jellyfin-api-key \
  --user-id vincent \
  --source-root /neo/videos \
  --dest-host aix.sbr.pm \
  --dest-user vincent \
  --dest-root /data/favorites \
  --dry-run \
  --verbose
```

### Options

- `--jellyfin-url`: Jellyfin server URL (required)
- `--api-key`: Jellyfin API key (use --api-key-file for secrets)
- `--api-key-file`: Path to file containing API key (recommended)
- `--user-id`: Jellyfin user ID or username (required)
- `--source-root`: Root path of Jellyfin library (default: /neo/videos)
- `--dest-host`: Destination SSH host (required)
- `--dest-user`: SSH user (default: vincent)
- `--dest-root`: Destination path (default: /data/favorites)
- `--ssh-arg`: Additional SSH arguments (can be repeated)
- `--dry-run`: Show operations without executing
- `--verbose`: Enable verbose output

### NixOS Service

Configure as a systemd service with scheduled execution:

```nix
services.jellyfin-favorites-sync = {
  enable = true;
  schedule = "daily";

  jellyfinUrl = "http://localhost:8096";
  apiKeyFile = config.age.secrets."jellyfin-favorites-sync-api-key".path;
  userId = "vincent";

  sourceRoot = "/neo/videos";

  destination = {
    host = "aix.sbr.pm";
    user = "vincent";
    root = "/data/favorites";
  };

  sshArgs = [ "-o StrictHostKeyChecking=accept-new" ];
};
```

## How It Works

1. **Connect to Jellyfin**: Authenticate using API key
2. **Query Favorites**: Fetch all favorited movies and series
3. **Expand Series**: For each favorited series, fetch all episodes
4. **Discover Paths**: Extract parent directories containing media + metadata
5. **Generate File List**: Create rsync `--files-from` input
6. **Execute Rsync**: Sync to remote host with `--delete` for mirror mode

## Examples

### Dry-Run to See What Would Be Synced

```bash
jellyfin-favorites-sync \
  --jellyfin-url https://jellyfin.sbr.pm \
  --api-key-file ~/.secrets/jellyfin-api-key \
  --user-id vincent \
  --dest-host aix.sbr.pm \
  --dry-run \
  --verbose
```

### Sync to Different Host

```bash
jellyfin-favorites-sync \
  --jellyfin-url https://jellyfin.sbr.pm \
  --api-key-file /run/agenix/jellyfin-api-key \
  --user-id vincent \
  --dest-host backup.example.com \
  --dest-root /backups/jellyfin-favorites
```

### With Custom SSH Port

```bash
jellyfin-favorites-sync \
  --jellyfin-url https://jellyfin.sbr.pm \
  --api-key-file ~/.secrets/jellyfin-api-key \
  --user-id vincent \
  --dest-host aix.sbr.pm \
  --ssh-arg "-p" \
  --ssh-arg "2222"
```

## Requirements

- Python 3.11+
- `requests` library
- `click` library
- `rsync` command
- `openssh` (for SSH transport)

## Security

- **API Key**: Stored in encrypted file via agenix, never logged
- **Path Validation**: All paths validated within source root
- **SSH**: Dedicated service user with restricted SSH key recommended

## Troubleshooting

### No Favorites Found

- Verify user ID is correct (run `jellyfin-favorites-sync --verbose`)
- Check that items are actually marked as favorite in Jellyfin UI
- Ensure API key has correct permissions

### Rsync Fails

- Verify SSH connectivity: `ssh dest-user@dest-host`
- Check destination path exists and has correct permissions
- Use `--verbose` to see full rsync command

### Series Not Expanding

- Check Jellyfin logs for API errors
- Verify series has episodes in Jellyfin library
- Use `--verbose` to see API responses

## License

MIT
