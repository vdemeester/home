# DebugContainer Workflow

Debug and troubleshoot running or failed containers.

## Process

1. **Detect Runtime**
   - Run `DetectRuntime.sh` to determine Docker/Podman

2. **Identify Container**
   - List containers to find the target
   - Get container name or ID

3. **Determine Debug Approach**
   - View logs
   - Inspect container configuration
   - Execute commands inside container
   - Check resource usage
   - Examine network connectivity
   - Review filesystem

4. **Execute Debug Commands**
   - Run appropriate diagnostic commands
   - Gather relevant information

5. **Analyze and Report**
   - Identify potential issues
   - Suggest solutions

## View Logs

**Show container logs:**
```bash
docker logs container_name
podman logs container_name
```

**Follow logs (real-time):**
```bash
docker logs -f container_name
podman logs -f container_name
```

**Show last N lines:**
```bash
docker logs --tail 100 container_name
podman logs --tail 100 container_name
```

**Show logs with timestamps:**
```bash
docker logs -t container_name
podman logs -t container_name
```

**Show logs since specific time:**
```bash
docker logs --since 2024-01-01T10:00:00 container_name
docker logs --since 1h container_name
podman logs --since 1h container_name
```

**Search logs:**
```bash
docker logs container_name 2>&1 | grep ERROR
podman logs container_name 2>&1 | grep ERROR
```

## Execute Commands

**Open shell in running container:**
```bash
docker exec -it container_name /bin/bash
docker exec -it container_name /bin/sh  # if bash not available
podman exec -it container_name /bin/bash
```

**Run specific command:**
```bash
docker exec container_name ls -la /app
docker exec container_name ps aux
podman exec container_name cat /etc/os-release
```

**Run as specific user:**
```bash
docker exec -u www-data -it container_name bash
podman exec -u 1000 -it container_name bash
```

**Set environment variables:**
```bash
docker exec -e DEBUG=true container_name python script.py
podman exec -e DEBUG=true container_name python script.py
```

## Inspect Container

**Full container details:**
```bash
docker inspect container_name
podman inspect container_name
```

**Specific field:**
```bash
# Get IP address
docker inspect -f '{{range.NetworkSettings.Networks}}{{.IPAddress}}{{end}}' container_name

# Get state
docker inspect -f '{{.State.Status}}' container_name

# Get exit code
docker inspect -f '{{.State.ExitCode}}' container_name

# Get environment variables
docker inspect -f '{{.Config.Env}}' container_name
```

**Container configuration:**
```bash
# Mounts
docker inspect -f '{{.Mounts}}' container_name

# Ports
docker inspect -f '{{.NetworkSettings.Ports}}' container_name

# Command
docker inspect -f '{{.Config.Cmd}}' container_name
```

## Monitor Resources

**Real-time stats:**
```bash
docker stats container_name
podman stats container_name
```

**All containers stats:**
```bash
docker stats
podman stats
```

**Stats without streaming:**
```bash
docker stats --no-stream
podman stats --no-stream
```

**Format output:**
```bash
docker stats --format "table {{.Container}}\t{{.CPUPerc}}\t{{.MemUsage}}"
```

## Check Processes

**List processes in container:**
```bash
docker top container_name
podman top container_name
```

**Detailed process info:**
```bash
docker top container_name aux
podman top container_name aux
```

## Network Debugging

**Check container IP:**
```bash
docker inspect -f '{{range.NetworkSettings.Networks}}{{.IPAddress}}{{end}}' container_name
```

**List networks:**
```bash
docker network ls
podman network ls
```

**Inspect network:**
```bash
docker network inspect bridge
podman network inspect podman
```

**Test connectivity from container:**
```bash
docker exec container_name ping google.com
docker exec container_name curl http://api.example.com
docker exec container_name nc -zv database 5432
```

**Check DNS resolution:**
```bash
docker exec container_name nslookup database
docker exec container_name cat /etc/resolv.conf
```

**Port mapping:**
```bash
docker port container_name
podman port container_name
```

## Filesystem Debugging

**Copy files from container:**
```bash
docker cp container_name:/path/to/file ./local/path
podman cp container_name:/path/to/file ./local/path
```

**Copy files to container:**
```bash
docker cp ./local/file container_name:/path/to/destination
podman cp ./local/file container_name:/path/to/destination
```

**Check disk usage in container:**
```bash
docker exec container_name df -h
docker exec container_name du -sh /var/log
```

**View file contents:**
```bash
docker exec container_name cat /etc/config.yml
docker exec container_name tail -f /var/log/app.log
```

## Container Events

**Watch events:**
```bash
docker events
podman events
```

**Filter events for specific container:**
```bash
docker events --filter container=container_name
podman events --filter container=container_name
```

**Filter by event type:**
```bash
docker events --filter event=start
docker events --filter event=die
```

## Check Health

**If healthcheck configured:**
```bash
docker inspect -f '{{.State.Health.Status}}' container_name
docker inspect -f '{{json .State.Health}}' container_name | jq
```

## Common Issues and Solutions

### Container Exits Immediately

**Check exit code:**
```bash
docker inspect -f '{{.State.ExitCode}}' container_name
```

**Common exit codes:**
- 0: Success
- 1: Application error
- 126: Command cannot be invoked
- 127: Command not found
- 137: SIGKILL (killed by system, often OOM)
- 139: SIGSEGV (segmentation fault)
- 143: SIGTERM (graceful termination)

**Check logs:**
```bash
docker logs container_name
```

**Run interactively to debug:**
```bash
docker run -it --entrypoint /bin/bash image_name
```

### Permission Issues

**Check user:**
```bash
docker exec container_name whoami
docker exec container_name id
```

**Check file permissions:**
```bash
docker exec container_name ls -la /app
```

**Run as root:**
```bash
docker exec -u 0 -it container_name bash
```

### Network Connectivity Issues

**Test from host to container:**
```bash
# Get container IP
IP=$(docker inspect -f '{{range.NetworkSettings.Networks}}{{.IPAddress}}{{end}}' container_name)
ping $IP
curl http://$IP:8080
```

**Test from container to outside:**
```bash
docker exec container_name ping 8.8.8.8
docker exec container_name curl https://google.com
```

**Check port bindings:**
```bash
docker port container_name
netstat -tlnp | grep 8080
```

### Memory/CPU Issues

**Check resources:**
```bash
docker stats container_name
```

**Check if OOM killed:**
```bash
docker inspect -f '{{.State.OOMKilled}}' container_name
dmesg | grep -i oom
```

**Set resource limits:**
```bash
docker run --memory="512m" --cpus="1.0" image_name
```

### Volume/Mount Issues

**Check mounts:**
```bash
docker inspect -f '{{json .Mounts}}' container_name | jq
```

**Verify volume exists:**
```bash
docker volume ls
docker volume inspect volume_name
```

**Check permissions:**
```bash
docker exec container_name ls -la /mounted/path
```

## Advanced Debugging

### Attach to Container

**Attach to running container (see STDOUT/STDERR):**
```bash
docker attach container_name
# Detach: Ctrl+P, Ctrl+Q
```

### Use nsenter (Advanced)

**Enter container namespaces:**
```bash
# Get container PID
PID=$(docker inspect -f '{{.State.Pid}}' container_name)

# Enter namespaces
nsenter -t $PID -n -u -i -p /bin/bash
```

### Strace Container Process

**Trace system calls:**
```bash
docker exec container_name strace -p 1
```

### Export Container Filesystem

**Export for offline analysis:**
```bash
docker export container_name > container.tar
tar -xf container.tar
```

### Commit Container State

**Save current state as image:**
```bash
docker commit container_name debug_snapshot:latest
```

## Podman-Specific Debugging

### Check Rootless Mode

```bash
podman info --format '{{.Host.Security.Rootless}}'
```

### Troubleshoot Rootless

```bash
# Check subuid/subgid
cat /etc/subuid
cat /etc/subgid

# Migrate storage if needed
podman system migrate
```

### Pod Debugging

```bash
# List pods
podman pod ps

# Inspect pod
podman pod inspect pod_name

# Logs for all containers in pod
podman pod logs pod_name
```

## Output Format

After debugging session:
```
✓ Debug information gathered
  Runtime: docker/podman
  Container: myapp
  Status: running
  Uptime: 2h 15m

  Findings:
    - CPU Usage: 45%
    - Memory: 256MB / 512MB
    - Network: Connected to bridge
    - Recent errors: 3 in last hour

  Logs (last 10 lines):
    [2024-01-01 10:00:00] INFO: Application started
    [2024-01-01 10:05:30] WARN: Database connection slow
    [2024-01-01 10:10:15] ERROR: Failed to connect to Redis

  Recommendations:
    - Check Redis connectivity
    - Consider increasing database timeout
    - Monitor memory usage (50% utilized)
```

## Quick Reference

```bash
# Quick debug checklist
docker logs container_name              # Check logs
docker inspect container_name           # Full details
docker exec -it container_name bash     # Get shell
docker stats container_name             # Resource usage
docker top container_name               # Processes
docker port container_name              # Port mappings
```
