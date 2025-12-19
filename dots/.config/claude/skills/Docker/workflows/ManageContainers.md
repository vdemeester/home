# ManageContainers Workflow

Start, stop, restart, inspect, and manage container lifecycle.

## Process

1. **Detect Runtime**
   - Run `DetectRuntime.sh` to determine Docker/Podman
   - Check for running containers

2. **Determine Action**
   - List containers
   - Start new container
   - Stop/restart existing container
   - Remove container
   - Inspect container details

3. **Execute Container Management**
   - Run appropriate commands
   - Handle container naming
   - Manage ports and volumes
   - Set environment variables

4. **Verify State**
   - Confirm container status
   - Show logs if issues occur
   - Display relevant information

## Common Operations

### List Containers

**Show all running containers:**
```bash
docker ps
podman ps
```

**Show all containers (including stopped):**
```bash
docker ps -a
podman ps -a
```

**Format output:**
```bash
docker ps --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"
podman ps --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"
```

### Start Containers

**Run new container:**
```bash
docker run -d --name myapp -p 8080:80 myapp:latest
podman run -d --name myapp -p 8080:80 myapp:latest
```

**Run with environment variables:**
```bash
docker run -d --name myapp -e KEY=value myapp:latest
podman run -d --name myapp -e KEY=value myapp:latest
```

**Run with volume mount:**
```bash
docker run -d --name myapp -v /host/path:/container/path myapp:latest
podman run -d --name myapp -v /host/path:/container/path myapp:latest
```

**Run interactively:**
```bash
docker run -it --name myapp myapp:latest /bin/bash
podman run -it --name myapp myapp:latest /bin/bash
```

### Stop/Restart Containers

**Stop container:**
```bash
docker stop myapp
podman stop myapp
```

**Stop with timeout:**
```bash
docker stop -t 30 myapp
podman stop -t 30 myapp
```

**Restart container:**
```bash
docker restart myapp
podman restart myapp
```

**Kill container (forceful):**
```bash
docker kill myapp
podman kill myapp
```

### Remove Containers

**Remove stopped container:**
```bash
docker rm myapp
podman rm myapp
```

**Force remove running container:**
```bash
docker rm -f myapp
podman rm -f myapp
```

**Remove all stopped containers:**
```bash
docker container prune
podman container prune
```

### Inspect Containers

**Show detailed information:**
```bash
docker inspect myapp
podman inspect myapp
```

**Get specific field:**
```bash
docker inspect -f '{{.State.Status}}' myapp
podman inspect -f '{{.State.Status}}' myapp
```

**Show resource usage:**
```bash
docker stats myapp
podman stats myapp
```

## Podman-Specific Features

### Rootless Containers
```bash
# Run rootless (default in Podman)
podman run -d --name myapp myapp:latest

# Check if rootless
podman info --format '{{.Host.Security.Rootless}}'
```

### Systemd Integration
```bash
# Generate systemd unit file
podman generate systemd --name myapp > ~/.config/systemd/user/myapp.service

# Enable and start service
systemctl --user enable --now myapp.service
```

### Pods (Kubernetes-like)
```bash
# Create pod
podman pod create --name mypod -p 8080:80

# Add container to pod
podman run -d --pod mypod --name myapp myapp:latest

# List pods
podman pod ps

# Stop entire pod
podman pod stop mypod
```

## Best Practices

### Container Naming
- Use descriptive names
- Follow consistent naming convention
- Avoid generic names like "app" or "container"

### Resource Management
- Set memory limits: `--memory 512m`
- Set CPU limits: `--cpus 1.5`
- Use health checks: `--health-cmd`

### Security
- Run as non-root when possible (Podman rootless)
- Use read-only filesystem: `--read-only`
- Drop capabilities: `--cap-drop ALL`
- Use security profiles: `--security-opt`

### Restart Policies
```bash
# Always restart
docker run -d --restart always myapp

# Restart on failure
docker run -d --restart on-failure:5 myapp

# No restart
docker run -d --restart no myapp
```

## Common Issues

**Issue**: Port already in use
- Check what's using the port: `lsof -i :8080` or `ss -tlnp | grep 8080`
- Use different host port: `-p 8081:80`

**Issue**: Container exits immediately
- Check logs: [trigger DebugContainer]
- Run interactively to debug: `-it`

**Issue**: Permission denied (Docker)
- Add user to docker group: `sudo usermod -aG docker $USER`
- Or use Podman rootless

**Issue**: Cannot connect to container
- Check port mapping: `docker port myapp`
- Verify network: `docker network inspect bridge`

## Output Format

After container operation:
```
✓ Container started successfully
  Runtime: docker/podman
  Name: myapp
  Status: running
  Ports: 0.0.0.0:8080->80/tcp
  Uptime: 5s

Next steps:
  - Check logs: docker logs myapp
  - Access application: curl http://localhost:8080
  - View stats: docker stats myapp
```
