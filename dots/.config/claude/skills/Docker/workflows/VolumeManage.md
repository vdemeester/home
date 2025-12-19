# VolumeManage Workflow

Create, manage, and troubleshoot persistent storage volumes for containers.

## Process

1. **Detect Runtime**
   - Run `DetectRuntime.sh` to determine Docker/Podman

2. **Determine Volume Operation**
   - List volumes
   - Create volume
   - Mount volume to container
   - Inspect volume
   - Backup/restore volume
   - Remove volume

3. **Execute Volume Command**
   - Run appropriate volume command
   - Configure volume settings

4. **Verify Configuration**
   - Check volume exists
   - Verify data persistence

## Types of Persistent Storage

### Named Volumes (Recommended)

**Managed by Docker/Podman:**
- Stored in Docker/Podman-managed location
- Easy to backup and migrate
- Work across platforms
- Best for most use cases

### Bind Mounts

**Direct host path mapping:**
- Mount specific host directory
- Useful for development
- Direct file access from host
- Platform-specific paths

### tmpfs Mounts (Linux only)

**In-memory storage:**
- Stored in host memory
- Not persisted to disk
- Fast but temporary
- Good for sensitive data

## List Volumes

**Show all volumes:**
```bash
docker volume ls
podman volume ls
```

**Filter volumes:**
```bash
# By name
docker volume ls --filter name=myapp
podman volume ls --filter name=myapp

# By driver
docker volume ls --filter driver=local
podman volume ls --filter driver=local

# Dangling (not attached to container)
docker volume ls --filter dangling=true
podman volume ls --filter dangling=true
```

## Create Volumes

### Named Volumes

**Create volume:**
```bash
docker volume create myvolume
podman volume create myvolume
```

**Create with driver options:**
```bash
docker volume create \
  --driver local \
  --opt type=nfs \
  --opt o=addr=192.168.1.1,rw \
  --opt device=:/path/to/dir \
  nfs-volume
```

**Create with labels:**
```bash
docker volume create \
  --label project=myapp \
  --label environment=production \
  myvolume

podman volume create \
  --label project=myapp \
  myvolume
```

## Mount Volumes

### Named Volume Mount

**Mount to container:**
```bash
docker run -v myvolume:/data myapp
docker run --mount source=myvolume,target=/data myapp

podman run -v myvolume:/data myapp
podman run --mount type=volume,source=myvolume,target=/data myapp
```

**Mount read-only:**
```bash
docker run -v myvolume:/data:ro myapp
docker run --mount source=myvolume,target=/data,readonly myapp

podman run -v myvolume:/data:ro myapp
```

**Mount with specific options:**
```bash
docker run --mount type=volume,source=myvolume,target=/data,volume-opt=o=size=100m myapp
```

### Bind Mount

**Mount host directory:**
```bash
docker run -v /host/path:/container/path myapp
docker run --mount type=bind,source=/host/path,target=/container/path myapp

podman run -v /host/path:/container/path myapp
podman run --mount type=bind,source=/host/path,target=/container/path myapp
```

**Bind mount read-only:**
```bash
docker run -v /host/path:/container/path:ro myapp
podman run -v /host/path:/container/path:ro myapp
```

**Bind mount with propagation:**
```bash
# Shared propagation
docker run -v /host/path:/container/path:shared myapp

# Private propagation (default)
docker run -v /host/path:/container/path:private myapp

# Slave propagation
docker run -v /host/path:/container/path:slave myapp
```

### tmpfs Mount

**Create tmpfs mount:**
```bash
docker run --tmpfs /app/cache myapp
docker run --mount type=tmpfs,target=/app/cache myapp

podman run --tmpfs /app/cache myapp
```

**With size limit:**
```bash
docker run --tmpfs /app/cache:size=100m myapp
docker run --mount type=tmpfs,target=/app/cache,tmpfs-size=100m myapp
```

## Inspect Volumes

**View volume details:**
```bash
docker volume inspect myvolume
podman volume inspect myvolume
```

**Get specific field:**
```bash
# Get mountpoint
docker volume inspect -f '{{.Mountpoint}}' myvolume
podman volume inspect -f '{{.Mountpoint}}' myvolume

# Get driver
docker volume inspect -f '{{.Driver}}' myvolume

# Get labels
docker volume inspect -f '{{.Labels}}' myvolume
```

**JSON output with jq:**
```bash
docker volume inspect myvolume | jq '.[0].Mountpoint'
podman volume inspect myvolume | jq '.[] | .Mountpoint'
```

## Remove Volumes

**Remove volume:**
```bash
docker volume rm myvolume
podman volume rm myvolume
```

**Force remove (if in use):**
```bash
docker volume rm -f myvolume
podman volume rm -f myvolume
```

**Remove multiple volumes:**
```bash
docker volume rm volume1 volume2 volume3
podman volume rm volume1 volume2 volume3
```

**Remove all unused volumes:**
```bash
docker volume prune
podman volume prune
```

**Filter and remove:**
```bash
# Remove dangling volumes
docker volume prune --filter "dangling=true"

# Remove volumes with specific label
docker volume prune --filter "label=project=old"
```

## Backup and Restore

### Backup Volume

**Backup to tar archive:**
```bash
# Create temporary container to backup volume
docker run --rm \
  -v myvolume:/data \
  -v $(pwd):/backup \
  alpine \
  tar czf /backup/myvolume-backup.tar.gz -C /data .

podman run --rm \
  -v myvolume:/data \
  -v $(pwd):/backup \
  alpine \
  tar czf /backup/myvolume-backup.tar.gz -C /data .
```

**Backup with timestamp:**
```bash
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
docker run --rm \
  -v myvolume:/data \
  -v $(pwd):/backup \
  alpine \
  tar czf /backup/myvolume-${TIMESTAMP}.tar.gz -C /data .
```

### Restore Volume

**Restore from tar archive:**
```bash
# Create volume if it doesn't exist
docker volume create myvolume

# Restore data
docker run --rm \
  -v myvolume:/data \
  -v $(pwd):/backup \
  alpine \
  tar xzf /backup/myvolume-backup.tar.gz -C /data

podman run --rm \
  -v myvolume:/data \
  -v $(pwd):/backup \
  alpine \
  tar xzf /backup/myvolume-backup.tar.gz -C /data
```

### Copy Between Volumes

**Copy data from one volume to another:**
```bash
docker run --rm \
  -v source-volume:/source:ro \
  -v dest-volume:/dest \
  alpine \
  cp -av /source/. /dest/
```

### Export Volume to Host

**Copy volume contents to host:**
```bash
docker run --rm \
  -v myvolume:/data \
  -v $(pwd)/export:/export \
  alpine \
  cp -av /data/. /export/
```

## Access Volume Data

### Direct Access (Advanced)

**Find volume location:**
```bash
# Docker (requires root)
docker volume inspect -f '{{.Mountpoint}}' myvolume
# Usually: /var/lib/docker/volumes/myvolume/_data

# Podman (rootless)
podman volume inspect -f '{{.Mountpoint}}' myvolume
# Usually: ~/.local/share/containers/storage/volumes/myvolume/_data
```

**Access data directly (requires appropriate permissions):**
```bash
# Docker (requires root)
sudo ls -la $(docker volume inspect -f '{{.Mountpoint}}' myvolume)

# Podman (rootless)
ls -la $(podman volume inspect -f '{{.Mountpoint}}' myvolume)
```

### Use Helper Container

**Browse volume contents:**
```bash
docker run -it --rm \
  -v myvolume:/data \
  alpine \
  sh

# Now you can explore /data
```

## Volume Drivers

### Local Driver (Default)

**Standard local storage:**
```bash
docker volume create --driver local myvolume
```

**With options:**
```bash
docker volume create \
  --driver local \
  --opt type=none \
  --opt device=/path/to/host/dir \
  --opt o=bind \
  myvolume
```

### NFS Driver

**Create NFS volume:**
```bash
docker volume create \
  --driver local \
  --opt type=nfs \
  --opt o=addr=192.168.1.100,rw,nfsvers=4 \
  --opt device=:/exported/path \
  nfs-volume
```

### Third-Party Drivers

**Examples of available drivers:**
- REX-Ray (cloud storage)
- Convoy
- Flocker
- GlusterFS
- NetApp

## Containers and Volumes

### Check Volume Usage

**Find containers using a volume:**
```bash
docker ps -a --filter volume=myvolume
podman ps -a --filter volume=myvolume
```

**List volumes used by container:**
```bash
docker inspect -f '{{range .Mounts}}{{.Name}} {{.Destination}}{{"\n"}}{{end}}' container_name
podman inspect -f '{{range .Mounts}}{{.Name}} {{.Destination}}{{"\n"}}{{end}}' container_name
```

### Anonymous Volumes

**Created automatically:**
```dockerfile
# In Dockerfile
VOLUME /data
```

**When running:**
```bash
docker run -v /data myapp  # Creates anonymous volume
```

**List anonymous volumes:**
```bash
docker volume ls --filter dangling=true
```

## Docker Compose Volumes

### Define Volumes

```yaml
version: '3.8'

services:
  web:
    image: nginx
    volumes:
      - html-data:/usr/share/nginx/html
      - ./config:/etc/nginx/conf.d:ro
      - type: tmpfs
        target: /tmp

  db:
    image: postgres
    volumes:
      - postgres-data:/var/lib/postgresql/data

volumes:
  html-data:
  postgres-data:
    driver: local
    driver_opts:
      type: none
      device: /path/to/host/dir
      o: bind
```

### External Volumes

```yaml
volumes:
  external-vol:
    external: true
    name: my-existing-volume
```

## Best Practices

### When to Use Named Volumes

- Database data
- Application state
- Uploaded files
- Configuration that persists

### When to Use Bind Mounts

- Development (live code reload)
- Configuration files (from host)
- Log file access
- Sharing files with host

### When to Use tmpfs

- Temporary files
- Cache
- Sensitive data (not persisted)
- Session storage

### Volume Naming

- Use descriptive names: `postgres-data`, `app-uploads`
- Include app name: `myapp-database`, `myapp-cache`
- Use labels for organization
- Consistent naming convention

### Security

- Use read-only mounts when possible
- Limit bind mount scope
- Don't mount sensitive host directories
- Use appropriate permissions
- Consider using secrets for sensitive data

### Performance

- Named volumes are faster than bind mounts on macOS/Windows
- Use tmpfs for frequently accessed temporary data
- Place volumes on fast storage
- Consider volume drivers for distributed storage

## Common Issues

**Issue**: Permission denied in container
- **Cause**: User ID mismatch between host and container
- **Solution**:
  ```bash
  # Run container as specific user
  docker run --user $(id -u):$(id -g) -v myvolume:/data myapp

  # Or fix permissions in container
  docker run --rm -v myvolume:/data alpine chown -R 1000:1000 /data
  ```

**Issue**: Volume data disappeared
- **Cause**: Used anonymous volume or removed volume
- **Solution**: Always use named volumes for persistent data

**Issue**: Cannot remove volume
- **Cause**: Container still using it
- **Solution**: Remove container first or use `docker volume rm -f`

**Issue**: Bind mount not updating (macOS/Windows)
- **Cause**: File sync delay in Docker Desktop
- **Solution**: Use named volumes or configure file sharing

**Issue**: Volume full
- **Check size**:
  ```bash
  docker run --rm -v myvolume:/data alpine du -sh /data
  ```
- **Clean up**: Remove unnecessary files or increase volume size

## Podman-Specific Volumes

### Rootless Volumes

**Location:**
```bash
~/.local/share/containers/storage/volumes/
```

### Import/Export

**Export volume:**
```bash
podman volume export myvolume > myvolume.tar
```

**Import volume:**
```bash
podman volume import myvolume < myvolume.tar
```

## Output Format

After volume operation:
```
✓ Volume created successfully
  Runtime: docker/podman
  Name: myapp-data
  Driver: local
  Mountpoint: /var/lib/docker/volumes/myapp-data/_data

  Mount to container:
    docker run -v myapp-data:/data myapp

  Backup volume:
    docker run --rm -v myapp-data:/data -v $(pwd):/backup alpine tar czf /backup/backup.tar.gz -C /data .

  Inspect:
    docker volume inspect myapp-data
```

## Quick Reference

```bash
# Create volume
docker volume create myvolume

# List volumes
docker volume ls

# Inspect volume
docker volume inspect myvolume

# Mount to container
docker run -v myvolume:/data myapp

# Bind mount
docker run -v /host/path:/container/path myapp

# Backup volume
docker run --rm -v myvolume:/data -v $(pwd):/backup alpine tar czf /backup/backup.tar.gz -C /data .

# Restore volume
docker run --rm -v myvolume:/data -v $(pwd):/backup alpine tar xzf /backup/backup.tar.gz -C /data

# Remove volume
docker volume rm myvolume

# Clean up unused volumes
docker volume prune
```
