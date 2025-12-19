# BuildMultiArch Workflow

Build container images for multiple CPU architectures (AMD64, ARM64, ARM/v7, etc.).

## Process

1. **Detect Runtime and Builder**
   - Run `DetectRuntime.sh` to determine Docker/Podman
   - Check for buildx (Docker) or buildah (Podman)

2. **Determine Target Platforms**
   - Common: linux/amd64, linux/arm64, linux/arm/v7
   - User-specified platforms

3. **Setup Multi-Arch Builder**
   - Create/use buildx builder (Docker)
   - Configure QEMU for cross-compilation

4. **Build for Multiple Platforms**
   - Build image for each platform
   - Create manifest list
   - Optionally push to registry

5. **Verify Build**
   - Check manifest
   - Verify platforms

## Docker with Buildx

### Setup

**Install QEMU (if not present):**
```bash
docker run --privileged --rm tonistiigi/binfmt --install all
```

**Create builder:**
```bash
docker buildx create --name multiarch --driver docker-container --use
docker buildx inspect --bootstrap
```

**List available platforms:**
```bash
docker buildx inspect --bootstrap | grep Platforms
```

### Build Multi-Arch Image

**Build for multiple platforms:**
```bash
docker buildx build \
  --platform linux/amd64,linux/arm64,linux/arm/v7 \
  -t myregistry/myapp:latest \
  --push \
  .
```

**Build and load locally (single platform):**
```bash
docker buildx build \
  --platform linux/arm64 \
  -t myapp:arm64 \
  --load \
  .
```

**Build without pushing:**
```bash
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  -t myregistry/myapp:latest \
  .
```

**Build with output to registry:**
```bash
docker buildx build \
  --platform linux/amd64,linux/arm64,linux/arm/v7 \
  -t myregistry/myapp:latest \
  --push \
  .
```

### Advanced Buildx

**Use specific builder:**
```bash
docker buildx build \
  --builder multiarch \
  --platform linux/amd64,linux/arm64 \
  -t myapp:latest \
  .
```

**Build with cache:**
```bash
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  --cache-from type=registry,ref=myregistry/myapp:buildcache \
  --cache-to type=registry,ref=myregistry/myapp:buildcache,mode=max \
  -t myregistry/myapp:latest \
  --push \
  .
```

**Build different tags per platform:**
```bash
docker buildx build \
  --platform linux/amd64 \
  -t myregistry/myapp:amd64 \
  --push \
  .

docker buildx build \
  --platform linux/arm64 \
  -t myregistry/myapp:arm64 \
  --push \
  .
```

### Manage Builders

**List builders:**
```bash
docker buildx ls
```

**Remove builder:**
```bash
docker buildx rm multiarch
```

**Use default builder:**
```bash
docker buildx use default
```

## Podman with Buildah

### Setup

**Install QEMU:**
```bash
# On host system (requires root/sudo)
sudo dnf install qemu-user-static  # Fedora/RHEL
sudo apt install qemu-user-static  # Debian/Ubuntu

# Or with Podman
podman run --rm --privileged multiarch/qemu-user-static --reset -p yes
```

### Build Multi-Arch Image

**Build for specific platform:**
```bash
podman build \
  --platform linux/arm64 \
  -t myapp:arm64 \
  .
```

**Build for multiple platforms (requires manifest):**
```bash
# Build for each platform separately
podman build --platform linux/amd64 -t myapp:amd64 .
podman build --platform linux/arm64 -t myapp:arm64 .
podman build --platform linux/arm/v7 -t myapp:armv7 .

# Create manifest
podman manifest create myapp:latest

# Add images to manifest
podman manifest add myapp:latest myapp:amd64
podman manifest add myapp:latest myapp:arm64
podman manifest add myapp:latest myapp:armv7

# Push manifest
podman manifest push myapp:latest docker://myregistry/myapp:latest
```

**Using buildah directly:**
```bash
# Build for ARM64
buildah bud --arch arm64 -t myapp:arm64 .

# Build for AMD64
buildah bud --arch amd64 -t myapp:amd64 .
```

## Common Platforms

| Platform | Description | Use Case |
|----------|-------------|----------|
| linux/amd64 | 64-bit x86 | Standard servers, desktops |
| linux/arm64 | 64-bit ARM | Raspberry Pi 4, Apple Silicon, AWS Graviton |
| linux/arm/v7 | 32-bit ARM v7 | Raspberry Pi 2/3, older ARM devices |
| linux/arm/v6 | 32-bit ARM v6 | Raspberry Pi Zero/1 |
| linux/386 | 32-bit x86 | Legacy systems |
| linux/ppc64le | PowerPC 64-bit LE | IBM POWER systems |
| linux/s390x | IBM Z | Mainframes |

## Dockerfile Considerations

### Platform-Specific Build

**Using build arguments:**
```dockerfile
FROM --platform=$BUILDPLATFORM golang:1.21-alpine AS builder
ARG TARGETPLATFORM
ARG BUILDPLATFORM
ARG TARGETOS
ARG TARGETARCH

WORKDIR /app
COPY . .

# Build for target platform
RUN GOOS=$TARGETOS GOARCH=$TARGETARCH go build -o app .

FROM --platform=$TARGETPLATFORM alpine:latest
COPY --from=builder /app/app /app
ENTRYPOINT ["/app"]
```

**Platform-specific base images:**
```dockerfile
# Multi-stage with platform awareness
FROM --platform=$BUILDPLATFORM golang:1.21 AS builder
ARG TARGETARCH

WORKDIR /src
COPY . .
RUN CGO_ENABLED=0 GOARCH=$TARGETARCH go build -o app

FROM alpine:latest
COPY --from=builder /src/app /app
CMD ["/app"]
```

**Conditional platform logic:**
```dockerfile
FROM alpine:latest

ARG TARGETARCH

# Install platform-specific packages
RUN if [ "$TARGETARCH" = "arm64" ]; then \
      echo "Installing ARM64 packages"; \
    elif [ "$TARGETARCH" = "amd64" ]; then \
      echo "Installing AMD64 packages"; \
    fi

COPY app /app
CMD ["/app"]
```

## Verify Multi-Arch Images

### Inspect Manifest

**Docker:**
```bash
# Inspect manifest list
docker buildx imagetools inspect myregistry/myapp:latest

# Should show multiple platforms
docker manifest inspect myregistry/myapp:latest
```

**Podman:**
```bash
# Inspect manifest
podman manifest inspect myapp:latest

# Show manifest details with skopeo
skopeo inspect docker://myregistry/myapp:latest
```

### Test Platform-Specific Images

**Run specific platform:**
```bash
# Force ARM64 on AMD64 host
docker run --platform linux/arm64 myregistry/myapp:latest

# Force AMD64 on ARM64 host
docker run --platform linux/amd64 myregistry/myapp:latest
```

**Verify architecture inside container:**
```bash
docker run --rm myregistry/myapp:latest uname -m
# Should show: x86_64, aarch64, armv7l, etc.
```

## Best Practices

### Optimization

1. **Use multi-stage builds:**
   - Separate build and runtime stages
   - Smaller final images

2. **Leverage build cache:**
   - Use remote cache for faster builds
   - Order layers appropriately

3. **Platform-specific optimizations:**
   - Different base images for different architectures
   - Conditional compilation flags

### CI/CD Integration

**GitHub Actions:**
```yaml
- name: Set up QEMU
  uses: docker/setup-qemu-action@v2

- name: Set up Docker Buildx
  uses: docker/setup-buildx-action@v2

- name: Build and push
  uses: docker/build-push-action@v4
  with:
    platforms: linux/amd64,linux/arm64,linux/arm/v7
    push: true
    tags: myregistry/myapp:latest
```

**GitLab CI:**
```yaml
build-multiarch:
  image: docker:latest
  services:
    - docker:dind
  before_script:
    - docker buildx create --use
    - docker buildx install
  script:
    - docker buildx build
      --platform linux/amd64,linux/arm64
      -t myregistry/myapp:latest
      --push .
```

### Testing

- Test on actual hardware when possible
- Use QEMU for cross-platform testing
- Verify critical functionality on each platform
- Check performance characteristics

## Common Issues

**Issue**: Build very slow for non-native platforms
- **Cause**: QEMU emulation overhead
- **Solution**: Use native builders (build on ARM for ARM), or accept slower builds

**Issue**: Build fails with "exec format error"
- **Cause**: QEMU not configured
- **Solution**: Run `docker run --privileged --rm tonistiigi/binfmt --install all`

**Issue**: Cannot load multi-platform image locally
- **Cause**: Local load only supports single platform
- **Solution**: Use `--platform` to specify one platform, or push to registry

**Issue**: Different behavior on different platforms
- **Cause**: Platform-specific dependencies or binaries
- **Solution**: Review Dockerfile, ensure cross-platform compatibility

**Issue**: Manifest push fails
- **Cause**: Authentication or manifest format issues
- **Solution**: Verify registry supports manifest lists, check credentials

## Output Format

After multi-arch build:
```
✓ Multi-architecture image built successfully
  Runtime: docker buildx / podman
  Image: myregistry/myapp:latest

  Platforms built:
    - linux/amd64 (152 MB)
    - linux/arm64 (148 MB)
    - linux/arm/v7 (145 MB)

  Total build time: 8m 32s

  Manifest pushed to: myregistry/myapp:latest

  Verify:
    docker buildx imagetools inspect myregistry/myapp:latest

  Test platforms:
    docker run --platform linux/amd64 myregistry/myapp:latest
    docker run --platform linux/arm64 myregistry/myapp:latest
```

## Quick Reference

```bash
# Docker Buildx
docker buildx create --name multiarch --use
docker buildx build --platform linux/amd64,linux/arm64 -t myapp:latest --push .
docker buildx imagetools inspect myregistry/myapp:latest

# Podman Manifest
podman build --platform linux/amd64 -t myapp:amd64 .
podman build --platform linux/arm64 -t myapp:arm64 .
podman manifest create myapp:latest
podman manifest add myapp:latest myapp:amd64
podman manifest add myapp:latest myapp:arm64
podman manifest push myapp:latest docker://myregistry/myapp:latest
```
