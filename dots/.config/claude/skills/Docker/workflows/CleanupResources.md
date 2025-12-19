# CleanupResources Workflow

Clean up unused containers, images, volumes, and networks to reclaim disk space.

## Process

1. **Detect Runtime**
   - Run `DetectRuntime.sh` to determine Docker/Podman

2. **Assess Current Usage**
   - Show disk usage: `docker system df` / `podman system df`
   - Identify reclaimable space

3. **Determine Cleanup Scope**
   - Containers (stopped)
   - Images (unused/dangling)
   - Volumes (unused)
   - Networks (unused)
   - Build cache

4. **Preview What Will Be Removed**
   - Show items to be removed
   - Estimate space to be reclaimed
   - **Ask for user confirmation before proceeding**

5. **Execute Cleanup**
   - Run appropriate prune commands
   - Show space reclaimed

## Check Disk Usage

**System-wide overview:**
```bash
docker system df
podman system df
```

**Detailed view:**
```bash
docker system df -v
podman system df -v
```

## Cleanup Operations

### Containers

**Remove all stopped containers:**
```bash
docker container prune
podman container prune
```

**Remove specific container:**
```bash
docker rm container_name
podman rm container_name
```

**Remove all containers (dangerous!):**
```bash
# Stop all first
docker stop $(docker ps -aq)
docker rm $(docker ps -aq)

podman stop -a
podman rm -a
```

### Images

**Remove dangling images (untagged):**
```bash
docker image prune
podman image prune
```

**Remove unused images:**
```bash
docker image prune -a
podman image prune -a
```

**Remove specific image:**
```bash
docker rmi image_name:tag
podman rmi image_name:tag
```

**Remove all images (dangerous!):**
```bash
docker rmi $(docker images -q)
podman rmi -a
```

### Volumes

**Remove unused volumes:**
```bash
docker volume prune
podman volume prune
```

**Remove specific volume:**
```bash
docker volume rm volume_name
podman volume rm volume_name
```

**Remove all volumes (dangerous!):**
```bash
docker volume rm $(docker volume ls -q)
podman volume rm -a
```

### Networks

**Remove unused networks:**
```bash
docker network prune
podman network prune
```

**Remove specific network:**
```bash
docker network rm network_name
podman network rm network_name
```

### Build Cache

**Clear build cache:**
```bash
docker builder prune
podman system prune --all --volumes
```

**Clear all build cache (more aggressive):**
```bash
docker builder prune -a
```

### Complete System Cleanup

**Prune everything (containers, images, volumes, networks):**
```bash
docker system prune -a --volumes
podman system prune -a --volumes
```

**With force (skip confirmation):**
```bash
docker system prune -a --volumes -f
podman system prune -a --volumes -f
```

## Safe Cleanup Strategy

### Step-by-Step Approach

1. **Start conservative:**
   ```bash
   # Remove stopped containers
   docker container prune

   # Remove dangling images
   docker image prune
   ```

2. **Then volumes:**
   ```bash
   # Review volumes first
   docker volume ls

   # Remove unused
   docker volume prune
   ```

3. **Finally unused images:**
   ```bash
   # This removes images not used by any container
   docker image prune -a
   ```

4. **Build cache (if needed):**
   ```bash
   docker builder prune
   ```

### Filter by Age

**Remove containers stopped more than 24 hours ago:**
```bash
docker container prune --filter "until=24h"
podman container prune --filter "until=24h"
```

**Remove images created more than 7 days ago:**
```bash
docker image prune -a --filter "until=168h"
podman image prune -a --filter "until=168h"
```

### Filter by Label

**Remove containers with specific label:**
```bash
docker container prune --filter "label=project=myapp"
podman container prune --filter "label=project=myapp"
```

## Best Practices

### Regular Maintenance
- Run cleanup weekly or monthly
- Use automation (cron job)
- Monitor disk usage

### Safety Checks
- **Always preview** what will be removed
- **Never use `-f` (force)** without understanding impact
- Keep tagged images you need
- Backup important volumes before cleanup

### Retention Strategy
```bash
# Keep images from last week
docker image prune -a --filter "until=168h"

# Keep containers stopped in last 24h
docker container prune --filter "until=24h"
```

## Automated Cleanup

### Docker System Prune with Cron

```bash
# Add to crontab (weekly cleanup)
0 2 * * 0 docker system prune -a --volumes -f >> /var/log/docker-cleanup.log 2>&1
```

### Podman Auto-Prune

```bash
# Podman rootless with systemd timer
systemctl --user enable --now podman-auto-update.timer
```

## Common Issues

**Issue**: Important data deleted
- **Prevention**: Always use named volumes for important data
- **Recovery**: Check volume backups if available

**Issue**: Still using too much space
- Check for large log files: `/var/lib/docker/containers/*/*-json.log`
- Configure log rotation:
  ```json
  {
    "log-driver": "json-file",
    "log-opts": {
      "max-size": "10m",
      "max-file": "3"
    }
  }
  ```

**Issue**: Cannot remove image in use
- Check which containers use it: `docker ps -a --filter ancestor=image_name`
- Stop and remove containers first

**Issue**: Permission denied (Docker)
- Use sudo or add user to docker group
- With Podman: run as regular user (rootless)

## Output Format

After cleanup:
```
✓ Cleanup completed successfully
  Runtime: docker/podman

  Removed:
    - Containers: 5
    - Images: 12
    - Volumes: 3
    - Networks: 2

  Space reclaimed: 2.5 GB

  Current usage:
    - Images: 5.2 GB
    - Containers: 150 MB
    - Volumes: 800 MB
    - Build cache: 1.1 GB
    - Total: 7.25 GB

Next steps:
  - Check usage: docker system df
  - Schedule regular cleanup
  - Configure log rotation
```

## Advanced Cleanup

### Clean Specific Image Tags

```bash
# Remove old versions of specific image
docker images myapp --format "{{.Tag}}" | grep -v "latest\|v1.0" | xargs -I {} docker rmi myapp:{}
```

### Clean by Pattern

```bash
# Remove all development images
docker images | grep "\-dev" | awk '{print $3}' | xargs docker rmi

# Remove unnamed images
docker images --filter "dangling=true" -q | xargs docker rmi
```

### Podman-Specific Cleanup

```bash
# Reset entire podman storage (nuclear option)
podman system reset

# Clean up pods
podman pod prune

# Clean up specific user's containers (rootless)
podman system prune --all
```
