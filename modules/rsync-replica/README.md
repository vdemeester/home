# rsync-replica Module

A NixOS module for declarative rsync-based replication between hosts.

## Features

- Declarative configuration of rsync replication jobs
- Systemd service and timer integration
- Support for multiple sync jobs
- Full archive mode with ACLs and extended attributes (-aAX)
- Optional deletion of files (true replica/mirror mode)
- Customizable rsync and SSH arguments
- Flexible scheduling (hourly, daily, weekly, or custom systemd OnCalendar)

## Usage

### Basic Configuration

```nix
services.rsync-replica = {
  enable = true;
  jobs = {
    backup-from-server = {
      source = {
        host = "source-hostname";
        user = "root";
        paths = [
          "/path/to/source1"
          "/path/to/source2"
        ];
      };
      destination = "/local/backup";
      schedule = "daily";
      delete = true;
    };
  };
};
```

### Multiple Jobs

```nix
services.rsync-replica = {
  enable = true;
  jobs = {
    media-sync = {
      source = {
        host = "media-server";
        paths = [ "/media/videos" "/media/music" ];
      };
      destination = "/backup/media";
      schedule = "daily";
    };

    documents-sync = {
      source = {
        host = "file-server";
        paths = [ "/documents" ];
      };
      destination = "/backup/documents";
      schedule = "hourly";
      rsyncArgs = [ "--exclude=*.tmp" "--bwlimit=5000" ];
    };
  };
};
```

## Options

### `services.rsync-replica.enable`
- Type: `boolean`
- Default: `false`
- Description: Enable the rsync-replica service

### `services.rsync-replica.jobs.<name>`
- Type: `attribute set`
- Description: Rsync replication jobs to run

#### Job Options

##### `source.host`
- Type: `string`
- Description: Remote hostname to sync from
- Example: `"rhea"`

##### `source.user`
- Type: `string`
- Default: `"root"`
- Description: SSH user to connect as

##### `source.paths`
- Type: `list of strings`
- Description: List of absolute paths on the source host to sync
- Example: `[ "/neo/videos" "/neo/pictures" ]`

##### `destination`
- Type: `string`
- Description: Local destination directory. Source basenames will be created here.
- Example: `"/neo"` (will create `/neo/videos` and `/neo/pictures`)

##### `schedule`
- Type: `string`
- Default: `"daily"`
- Description: When to run the sync. Can be "hourly", "daily", "weekly", or a systemd OnCalendar format.
- Example: `"daily"` or `"*-*-* 02:00:00"` (daily at 2 AM)

##### `delete`
- Type: `boolean`
- Default: `true`
- Description: Delete files in destination that don't exist in source (true replica/mirror mode)

##### `deleteExcluded`
- Type: `boolean`
- Default: `false`
- Description: Also delete excluded files from destination

##### `rsyncArgs`
- Type: `list of strings`
- Default: `[ ]`
- Description: Additional arguments to pass to rsync
- Example: `[ "--exclude=*.tmp" "--bwlimit=10000" ]`

##### `sshArgs`
- Type: `list of strings`
- Default: `[ ]`
- Description: Additional arguments to pass to SSH
- Example: `[ "-p 2222" "-i /root/.ssh/id_ed25519" ]`

##### `user`
- Type: `string`
- Default: `"root"`
- Description: User to run the rsync job as

##### `group`
- Type: `string`
- Default: `"root"`
- Description: Group to run the rsync job as

##### `randomizedDelay`
- Type: `string`
- Default: `"0"`
- Description: Randomized delay before starting the job (systemd RandomizedDelaySec)
- Example: `"1h"` (randomize start within 1 hour)

## How It Works

For each job, the module:
1. Creates a systemd service `rsync-replica-<job-name>`
2. Creates a systemd timer `rsync-replica-<job-name>` to trigger the service
3. For each path in `source.paths`, syncs `user@host:/path/` to `destination/basename/`

For example, if you have:
```nix
source = {
  host = "rhea";
  paths = [ "/neo/videos" "/neo/pictures" ];
};
destination = "/neo";
```

This will sync:
- `root@rhea:/neo/videos/` → `/neo/videos/`
- `root@rhea:/neo/pictures/` → `/neo/pictures/`

## Management Commands

```bash
# Check status of a sync job
systemctl status rsync-replica-<job-name>

# View timer schedule
systemctl list-timers rsync-replica-*

# Manually trigger a sync
systemctl start rsync-replica-<job-name>

# View logs
journalctl -u rsync-replica-<job-name>
```

## Prerequisites

- SSH access from the destination host to the source host
- SSH keys set up for passwordless authentication
- Sufficient disk space on destination
- Network connectivity between hosts

## Common Use Cases

### Daily Backup with Exclusions
```nix
services.rsync-replica.jobs.daily-backup = {
  source = {
    host = "production-server";
    paths = [ "/var/lib/important-data" ];
  };
  destination = "/backup";
  schedule = "daily";
  delete = true;
  rsyncArgs = [
    "--exclude=cache/"
    "--exclude=*.log"
    "--exclude=tmp/"
  ];
};
```

### Bandwidth-Limited Sync
```nix
services.rsync-replica.jobs.slow-sync = {
  source = {
    host = "remote-server";
    paths = [ "/large/dataset" ];
  };
  destination = "/local/copy";
  schedule = "weekly";
  rsyncArgs = [ "--bwlimit=1000" ]; # Limit to 1 MB/s
};
```

### Custom SSH Port
```nix
services.rsync-replica.jobs.custom-port = {
  source = {
    host = "server-with-custom-port";
    paths = [ "/data" ];
  };
  destination = "/backup";
  sshArgs = [ "-p 2222" ];
};
```
