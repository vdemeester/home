# OptimizeImage Workflow

Reduce container image size and improve build performance through optimization techniques.

## Process

1. **Analyze Current Image**
   - Check image size
   - Examine layers
   - Identify large files
   - Review Dockerfile

2. **Identify Optimization Opportunities**
   - Base image selection
   - Layer optimization
   - Build cache usage
   - Unnecessary files

3. **Apply Optimizations**
   - Use multi-stage builds
   - Minimize layers
   - Use smaller base images
   - Remove build artifacts

4. **Measure Improvements**
   - Compare before/after sizes
   - Verify functionality
   - Test performance

## Analyze Image

### Check Image Size

**View image size:**
```bash
docker images myapp
podman images myapp
```

**Show size in different formats:**
```bash
docker images --format "{{.Repository}}:{{.Tag}}\t{{.Size}}"
```

### Examine Layers

**View image history:**
```bash
docker history myapp:latest
docker history --no-trunc myapp:latest  # Full commands

podman history myapp:latest
```

**Analyze with dive:**
```bash
# Install dive
brew install dive
# Or: docker pull wagoodman/dive

# Analyze image
dive myapp:latest
```

**Find large layers:**
```bash
docker history myapp:latest --format "{{.Size}}\t{{.CreatedBy}}" | sort -h
```

## Base Image Optimization

### Choose Minimal Base Images

**Size comparison:**
```dockerfile
# Large (1.1 GB)
FROM ubuntu:latest

# Medium (130 MB)
FROM ubuntu:22.04

# Small (77 MB)
FROM debian:bookworm-slim

# Smaller (7 MB)
FROM alpine:3.19

# Smallest (< 2 MB)
FROM scratch  # For static binaries
FROM gcr.io/distroless/static-debian12  # For Go/Rust
```

### Language-Specific Minimal Images

**Python:**
```dockerfile
# Large (900 MB)
FROM python:3.11

# Small (50 MB)
FROM python:3.11-slim

# Smaller (45 MB)
FROM python:3.11-alpine
```

**Node.js:**
```dockerfile
# Large (1 GB)
FROM node:20

# Small (240 MB)
FROM node:20-slim

# Smaller (180 MB)
FROM node:20-alpine
```

**Go:**
```dockerfile
# For building
FROM golang:1.21-alpine AS builder

# For runtime (static binary)
FROM scratch
FROM gcr.io/distroless/static-debian12
```

## Multi-Stage Builds

### Basic Pattern

**Before (large):**
```dockerfile
FROM golang:1.21
WORKDIR /app
COPY . .
RUN go build -o myapp
CMD ["/app/myapp"]
# Result: ~1 GB
```

**After (small):**
```dockerfile
# Build stage
FROM golang:1.21-alpine AS builder
WORKDIR /app
COPY go.* ./
RUN go mod download
COPY . .
RUN go build -o myapp

# Runtime stage
FROM alpine:3.19
WORKDIR /app
COPY --from=builder /app/myapp .
CMD ["/app/myapp"]
# Result: ~20 MB
```

### Advanced Multi-Stage

**Multiple build stages:**
```dockerfile
# Stage 1: Dependencies
FROM node:20-alpine AS deps
WORKDIR /app
COPY package*.json ./
RUN npm ci --only=production

# Stage 2: Build
FROM node:20-alpine AS builder
WORKDIR /app
COPY package*.json ./
RUN npm ci
COPY . .
RUN npm run build

# Stage 3: Runtime
FROM node:20-alpine
WORKDIR /app
ENV NODE_ENV=production
COPY --from=deps /app/node_modules ./node_modules
COPY --from=builder /app/dist ./dist
COPY package.json ./
CMD ["node", "dist/index.js"]
```

## Layer Optimization

### Combine RUN Commands

**Before:**
```dockerfile
RUN apt-get update
RUN apt-get install -y curl
RUN apt-get install -y git
RUN apt-get clean
# Creates 4 layers
```

**After:**
```dockerfile
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        curl \
        git && \
    rm -rf /var/lib/apt/lists/*
# Creates 1 layer
```

### Order Layers by Change Frequency

**Optimize build cache:**
```dockerfile
# Bad: changes to code invalidate all layers
FROM node:20-alpine
WORKDIR /app
COPY . .
RUN npm install
RUN npm run build

# Good: dependencies cached separately
FROM node:20-alpine
WORKDIR /app
# Copy package files first (changes less frequently)
COPY package*.json ./
RUN npm ci
# Copy code last (changes most frequently)
COPY . .
RUN npm run build
```

### Clean Up in Same Layer

**Bad:**
```dockerfile
RUN wget https://example.com/large-file.tar.gz
RUN tar -xzf large-file.tar.gz
RUN rm large-file.tar.gz
# large-file.tar.gz still in first layer!
```

**Good:**
```dockerfile
RUN wget https://example.com/large-file.tar.gz && \
    tar -xzf large-file.tar.gz && \
    rm large-file.tar.gz
# File removed in same layer
```

## Remove Unnecessary Files

### Use .dockerignore

**Create .dockerignore:**
```dockerignore
# Version control
.git
.gitignore

# Dependencies
node_modules
vendor

# Build artifacts
dist
build
*.o
*.a

# Documentation
*.md
docs/

# Tests
*_test.go
test/
tests/
__tests__

# CI/CD
.github
.gitlab-ci.yml
Jenkinsfile

# Development
.env
.vscode
.idea
*.log

# OS files
.DS_Store
Thumbs.db
```

### Remove Package Manager Cache

**APT (Debian/Ubuntu):**
```dockerfile
RUN apt-get update && \
    apt-get install -y package && \
    rm -rf /var/lib/apt/lists/*
```

**YUM/DNF (RHEL/Fedora):**
```dockerfile
RUN dnf install -y package && \
    dnf clean all
```

**APK (Alpine):**
```dockerfile
RUN apk add --no-cache package
```

**npm:**
```dockerfile
RUN npm ci --only=production && \
    npm cache clean --force
```

**pip:**
```dockerfile
RUN pip install --no-cache-dir -r requirements.txt
```

## Minimize Installed Packages

### Install Only Required Packages

**Bad:**
```dockerfile
RUN apt-get update && \
    apt-get install -y build-essential
# Installs many unnecessary packages
```

**Good:**
```dockerfile
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        gcc \
        make && \
    rm -rf /var/lib/apt/lists/*
# Minimal installation
```

### Remove Build Dependencies

**Pattern for temporary build deps:**
```dockerfile
RUN apk add --no-cache --virtual .build-deps \
        gcc \
        musl-dev \
        postgresql-dev && \
    pip install psycopg2 && \
    apk del .build-deps
```

## Static Binaries

### Go Example

```dockerfile
# Build stage
FROM golang:1.21-alpine AS builder
WORKDIR /app
COPY . .
RUN CGO_ENABLED=0 GOOS=linux go build -a -installsuffix cgo -o myapp .

# Runtime stage
FROM scratch
COPY --from=builder /app/myapp /myapp
ENTRYPOINT ["/myapp"]
# Result: ~10 MB (just the binary!)
```

### Rust Example

```dockerfile
# Build stage
FROM rust:1.75-alpine AS builder
WORKDIR /app
COPY . .
RUN cargo build --release

# Runtime stage
FROM scratch
COPY --from=builder /app/target/release/myapp /myapp
ENTRYPOINT ["/myapp"]
```

## Compression and Squashing

### Use BuildKit Compression

**Enable BuildKit:**
```bash
export DOCKER_BUILDKIT=1
docker build --squash -t myapp:latest .
```

**Build with compression:**
```bash
docker build --compress -t myapp:latest .
```

### Squash Layers (Advanced)

**Flatten layers:**
```bash
docker build --squash -t myapp:latest .
```

**Note:** Squashing can reduce cache effectiveness.

## Distroless Images

### Using Distroless

**Java:**
```dockerfile
FROM gcr.io/distroless/java17-debian12
COPY target/app.jar /app.jar
CMD ["app.jar"]
```

**Python:**
```dockerfile
FROM gcr.io/distroless/python3-debian12
COPY --from=builder /app /app
WORKDIR /app
CMD ["app.py"]
```

**Node.js:**
```dockerfile
FROM gcr.io/distroless/nodejs20-debian12
COPY --from=builder /app /app
WORKDIR /app
CMD ["index.js"]
```

**Static binaries:**
```dockerfile
FROM gcr.io/distroless/static-debian12
COPY --from=builder /app/myapp /
CMD ["/myapp"]
```

## Optimization Checklist

### Base Image
- [ ] Use minimal base image (alpine, slim, distroless)
- [ ] Use specific version tags, not `latest`
- [ ] Consider distroless for production

### Dockerfile Structure
- [ ] Use multi-stage builds
- [ ] Order layers by change frequency
- [ ] Combine RUN commands
- [ ] Clean up in same layer

### Dependencies
- [ ] Install only required packages
- [ ] Use `--no-install-recommends` (apt)
- [ ] Use `--no-cache` (apk)
- [ ] Remove package manager cache
- [ ] Remove build dependencies after use

### Files
- [ ] Create .dockerignore file
- [ ] Exclude development files
- [ ] Exclude documentation
- [ ] Don't include .git directory

### Security & Best Practices
- [ ] Run as non-root user
- [ ] Don't include secrets
- [ ] Update packages during build
- [ ] Use specific versions

## Measurement

### Compare Image Sizes

```bash
# Build original
docker build -t myapp:old -f Dockerfile.old .

# Build optimized
docker build -t myapp:new -f Dockerfile.new .

# Compare
docker images | grep myapp
```

### Analyze Layer Sizes

```bash
# Detailed history
docker history myapp:old --no-trunc
docker history myapp:new --no-trunc

# Compare with dive
dive myapp:old
dive myapp:new
```

### Calculate Savings

```bash
# Simple calculation
OLD_SIZE=$(docker images myapp:old --format "{{.Size}}")
NEW_SIZE=$(docker images myapp:new --format "{{.Size}}")
echo "Old: $OLD_SIZE, New: $NEW_SIZE"
```

## Real-World Examples

### Python Flask App

**Before (500 MB):**
```dockerfile
FROM python:3.11
WORKDIR /app
COPY . .
RUN pip install -r requirements.txt
CMD ["python", "app.py"]
```

**After (50 MB):**
```dockerfile
FROM python:3.11-slim AS builder
WORKDIR /app
COPY requirements.txt .
RUN pip install --user --no-cache-dir -r requirements.txt

FROM python:3.11-slim
WORKDIR /app
COPY --from=builder /root/.local /root/.local
COPY app.py .
ENV PATH=/root/.local/bin:$PATH
CMD ["python", "app.py"]
```

### Node.js React App

**Before (1.2 GB):**
```dockerfile
FROM node:20
WORKDIR /app
COPY . .
RUN npm install
RUN npm run build
CMD ["npm", "start"]
```

**After (100 MB):**
```dockerfile
FROM node:20-alpine AS builder
WORKDIR /app
COPY package*.json ./
RUN npm ci
COPY . .
RUN npm run build

FROM nginx:alpine
COPY --from=builder /app/build /usr/share/nginx/html
EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]
```

### Go Microservice

**Before (800 MB):**
```dockerfile
FROM golang:1.21
WORKDIR /app
COPY . .
RUN go build -o myapp
CMD ["/app/myapp"]
```

**After (10 MB):**
```dockerfile
FROM golang:1.21-alpine AS builder
WORKDIR /app
COPY go.* ./
RUN go mod download
COPY . .
RUN CGO_ENABLED=0 go build -ldflags="-w -s" -o myapp

FROM scratch
COPY --from=builder /app/myapp /
CMD ["/myapp"]
```

## Advanced Techniques

### Link-Time Optimization

**Go:**
```dockerfile
RUN CGO_ENABLED=0 go build -ldflags="-w -s" -o myapp
# -w: Omit debug info
# -s: Omit symbol table
```

**Rust:**
```dockerfile
RUN cargo build --release
RUN strip target/release/myapp
```

### Use BuildKit Cache Mounts

```dockerfile
# syntax=docker/dockerfile:1
FROM golang:1.21-alpine
WORKDIR /app
COPY go.* ./
RUN --mount=type=cache,target=/go/pkg/mod \
    go mod download
COPY . .
RUN --mount=type=cache,target=/go/pkg/mod \
    --mount=type=cache,target=/root/.cache/go-build \
    go build -o myapp
```

## Common Mistakes

### Mistake 1: Installing Dev Dependencies

```dockerfile
# Bad
RUN npm install

# Good
RUN npm ci --only=production
```

### Mistake 2: Not Using .dockerignore

```dockerfile
# Without .dockerignore: copies everything including node_modules, .git
COPY . .

# With .dockerignore: copies only needed files
COPY . .  # But .dockerignore excludes unnecessary files
```

### Mistake 3: Cleaning Up in Different Layer

```dockerfile
# Bad: file remains in earlier layer
RUN wget large-file.tar.gz
RUN rm large-file.tar.gz

# Good: cleanup in same layer
RUN wget large-file.tar.gz && \
    tar -xzf large-file.tar.gz && \
    rm large-file.tar.gz
```

### Mistake 4: Using `latest` Tag

```dockerfile
# Bad: unpredictable, may break
FROM alpine:latest

# Good: specific, reproducible
FROM alpine:3.19
```

## Output Format

After optimization:
```
✓ Image optimized successfully
  Image: myapp:latest

  Size reduction:
    Before: 850 MB
    After: 45 MB
    Savings: 805 MB (94.7%)

  Layers:
    Before: 15 layers
    After: 5 layers

  Optimizations applied:
    - Changed base image: python:3.11 → python:3.11-slim
    - Added multi-stage build
    - Combined RUN commands (4 → 1)
    - Removed package cache
    - Added .dockerignore (excluded 250 MB)

  Build time:
    Before: 3m 45s
    After: 1m 20s (faster due to better caching)

  Next steps:
    - Test optimized image
    - Run security scan
    - Push to registry
```

## Quick Reference

```bash
# Analyze image
docker history myapp:latest
dive myapp:latest

# Use minimal base images
FROM alpine:3.19
FROM python:3.11-slim
FROM gcr.io/distroless/static-debian12

# Multi-stage build pattern
FROM golang:1.21-alpine AS builder
# ... build ...
FROM scratch
COPY --from=builder /app/binary /

# Combine and clean up
RUN apk add --no-cache package && \
    # do work && \
    apk del package

# Use .dockerignore
.git
node_modules
*.md
```
