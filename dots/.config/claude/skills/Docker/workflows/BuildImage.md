# BuildImage Workflow

Build container images with best practices for Docker or Podman.

## Process

1. **Detect Runtime**
   - Parse user prompt for explicit runtime mention ("use Docker", "with Podman")
   - If explicit runtime mentioned: Use `DetectRuntime.sh --runtime <docker|podman>`
   - If auto-detect: Run `DetectRuntime.sh` (prefers Podman, falls back to Docker)
   - Identify available build tools (buildx, buildah)

2. **Analyze Context**
   - Check for existing Dockerfile
   - If no Dockerfile exists, ask user if they want to create one
   - Review Dockerfile for best practices

3. **Determine Build Strategy**
   - Single-stage vs multi-stage build
   - Build arguments needed
   - Target platform(s)
   - Tags to apply

4. **Build Image**
   - Use detected runtime (docker/podman)
   - Apply appropriate tags
   - Show build progress
   - Handle build errors

5. **Post-Build Actions**
   - Display image size
   - Show image layers (if requested)
   - Suggest optimizations if image is large
   - Offer to run security scan

## Best Practices to Check

### Dockerfile Quality
- [ ] Uses specific base image tags (not `latest`)
- [ ] Minimizes number of layers
- [ ] Uses multi-stage builds where appropriate
- [ ] Runs as non-root user
- [ ] Has .dockerignore file
- [ ] No secrets embedded
- [ ] Proper layer ordering (least to most frequently changed)

### Build Optimization
- [ ] Leverages build cache effectively
- [ ] Combines RUN commands where appropriate
- [ ] Cleans up in the same layer
- [ ] Uses minimal base images

## Example Commands

### Docker
```bash
# Basic build
docker build -t myapp:latest .

# Build with arguments
docker build --build-arg VERSION=1.0 -t myapp:1.0 .

# Build with specific target
docker build --target production -t myapp:prod .

# Build with no cache
docker build --no-cache -t myapp:latest .
```

### Podman
```bash
# Basic build
podman build -t myapp:latest .

# Build with arguments
podman build --build-arg VERSION=1.0 -t myapp:1.0 .

# Build with specific target
podman build --target production -t myapp:prod .

# Build with no cache
podman build --no-cache -t myapp:latest .
```

## Common Issues

**Issue**: Build fails with permission denied
- **Docker**: User not in docker group or daemon not running
- **Podman**: Check rootless permissions, may need `podman system migrate`

**Issue**: Large image size
- Suggest multi-stage builds
- Recommend OptimizeImage workflow

**Issue**: Build cache not working
- Check layer ordering
- Verify .dockerignore excludes changing files

## Output Format

After successful build:
```
✓ Image built successfully
  Runtime: docker/podman
  Image: myapp:latest
  Size: 125 MB
  Build time: 45s

Next steps:
  - Run the image: docker run myapp:latest
  - Scan for vulnerabilities: [trigger SecurityScan]
  - Push to registry: [trigger RegistryManage]
```
