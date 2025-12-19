---
name: Docker
description: Container management with Docker and Podman. USE WHEN building images, managing containers, working with compose files, debugging containers, managing networks/volumes, scanning for vulnerabilities, or optimizing images.
---

# Docker

Comprehensive container management using Docker or Podman with intelligent runtime detection.

## Features

- **Runtime Detection**: Automatically detects and uses Docker or Podman
- **Compose Support**: Works with docker-compose and podman-compose
- **Multi-Architecture**: Build images for multiple platforms
- **Rootless Support**: Handles Podman rootless mode
- **Security**: Image vulnerability scanning and best practices
- **Optimization**: Image size reduction and layer optimization

## Workflow Routing

| Workflow | Trigger | File |
|----------|---------|------|
| **BuildImage** | "build docker image", "create dockerfile", "build container image" | `workflows/BuildImage.md` |
| **ManageContainers** | "start container", "stop container", "container status", "list containers" | `workflows/ManageContainers.md` |
| **ComposeManage** | "docker compose", "compose up", "manage compose services" | `workflows/ComposeManage.md` |
| **CleanupResources** | "clean docker", "remove unused images", "prune containers" | `workflows/CleanupResources.md` |
| **DebugContainer** | "container logs", "exec into container", "debug container" | `workflows/DebugContainer.md` |
| **BuildMultiArch** | "build arm image", "multi-arch build", "cross-platform image" | `workflows/BuildMultiArch.md` |
| **RegistryManage** | "push image", "pull image", "login to registry" | `workflows/RegistryManage.md` |
| **NetworkManage** | "create network", "list networks", "connect container to network" | `workflows/NetworkManage.md` |
| **VolumeManage** | "create volume", "list volumes", "mount volume" | `workflows/VolumeManage.md` |
| **SecurityScan** | "scan image", "check vulnerabilities", "security scan" | `workflows/SecurityScan.md` |
| **OptimizeImage** | "reduce image size", "optimize dockerfile", "smaller image" | `workflows/OptimizeImage.md` |

## Docker vs Podman

This skill works seamlessly with both runtimes:

### Docker
- Daemon-based architecture
- Requires root or docker group membership
- Native docker-compose support
- Standard on most systems

### Podman
- Daemonless, fork-exec model
- Rootless by default
- Drop-in Docker CLI replacement
- Systemd integration for containers
- Support for pods (Kubernetes-like)

The skill automatically detects which runtime is available and adjusts commands accordingly.

## Examples

**Example 1: Build a container image**
```
User: "Build a Docker image from the Dockerfile in the current directory"
→ Invokes BuildImage workflow
→ Detects Docker/Podman
→ Builds image with best practices
→ Tags appropriately
```

**Example 2: Debug a running container**
```
User: "Show me the logs for the nginx container"
→ Invokes DebugContainer workflow
→ Retrieves and displays logs
→ Offers to exec into container if needed
```

**Example 3: Clean up unused resources**
```
User: "Clean up old Docker images and containers"
→ Invokes CleanupResources workflow
→ Shows what will be removed
→ Prunes unused resources safely
```

**Example 4: Multi-architecture build**
```
User: "Build this image for both AMD64 and ARM64"
→ Invokes BuildMultiArch workflow
→ Sets up buildx/buildah
→ Builds for multiple platforms
→ Pushes to registry with manifest
```

## Best Practices

### Dockerfile
- Use multi-stage builds
- Minimize layers
- Use specific base image tags
- Run as non-root user
- Use .dockerignore

### Security
- Scan images regularly
- Keep base images updated
- Don't embed secrets
- Use minimal base images
- Follow least privilege principle

### Performance
- Leverage build cache
- Order layers by change frequency
- Use smaller base images
- Clean up in same layer

## Tools

- **DetectRuntime.sh**: Detects Docker or Podman and returns runtime info
