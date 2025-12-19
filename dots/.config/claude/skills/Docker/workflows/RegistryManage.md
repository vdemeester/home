# RegistryManage Workflow

Push, pull, and manage container images in registries (Docker Hub, GHCR, private registries).

## Process

1. **Detect Runtime**
   - Run `DetectRuntime.sh` to determine Docker/Podman

2. **Determine Registry**
   - Docker Hub (docker.io)
   - GitHub Container Registry (ghcr.io)
   - GitLab Container Registry (registry.gitlab.com)
   - Private/self-hosted registry
   - Cloud provider registries (ECR, GCR, ACR)

3. **Authenticate**
   - Login to registry
   - Handle credentials securely

4. **Execute Registry Operation**
   - Push images
   - Pull images
   - Search images
   - Delete images

5. **Verify**
   - Confirm operation success
   - Show image details

## Authentication

### Docker Hub

**Login:**
```bash
docker login
# Or with credentials
docker login -u username -p password

podman login docker.io
podman login -u username -p password docker.io
```

**Logout:**
```bash
docker logout
podman logout docker.io
```

### GitHub Container Registry

**Login with token:**
```bash
echo $GITHUB_TOKEN | docker login ghcr.io -u USERNAME --password-stdin
echo $GITHUB_TOKEN | podman login ghcr.io -u USERNAME --password-stdin
```

**Login interactive:**
```bash
docker login ghcr.io
podman login ghcr.io
```

### GitLab Container Registry

**Login:**
```bash
docker login registry.gitlab.com -u USERNAME -p $CI_JOB_TOKEN
podman login registry.gitlab.com -u USERNAME -p $CI_JOB_TOKEN
```

### Private Registry

**Login to private registry:**
```bash
docker login myregistry.example.com
podman login myregistry.example.com
```

**With self-signed certificate:**
```bash
# Docker: Add to /etc/docker/daemon.json
{
  "insecure-registries": ["myregistry.example.com:5000"]
}

# Podman: Use --tls-verify=false (not recommended for production)
podman login --tls-verify=false myregistry.example.com
```

### AWS ECR

**Login to ECR:**
```bash
aws ecr get-login-password --region us-east-1 | \
  docker login --username AWS --password-stdin 123456789.dkr.ecr.us-east-1.amazonaws.com

aws ecr get-login-password --region us-east-1 | \
  podman login --username AWS --password-stdin 123456789.dkr.ecr.us-east-1.amazonaws.com
```

### Google Container Registry

**Login to GCR:**
```bash
gcloud auth configure-docker

# Or manually
cat keyfile.json | docker login -u _json_key --password-stdin gcr.io
```

### Azure Container Registry

**Login to ACR:**
```bash
az acr login --name myregistry

# Or with service principal
docker login myregistry.azurecr.io -u $SP_ID -p $SP_PASSWORD
```

## Push Images

### Tag for Registry

**Tag image for registry:**
```bash
docker tag myapp:latest myregistry/myapp:latest
docker tag myapp:latest myregistry/myapp:v1.0.0

podman tag myapp:latest myregistry/myapp:latest
podman tag myapp:latest myregistry/myapp:v1.0.0
```

**Tag for Docker Hub:**
```bash
docker tag myapp:latest username/myapp:latest
podman tag myapp:latest docker.io/username/myapp:latest
```

**Tag for GHCR:**
```bash
docker tag myapp:latest ghcr.io/username/myapp:latest
podman tag myapp:latest ghcr.io/username/myapp:latest
```

### Push to Registry

**Push image:**
```bash
docker push myregistry/myapp:latest
podman push myregistry/myapp:latest
```

**Push all tags:**
```bash
docker push --all-tags myregistry/myapp
podman push --all-tags myregistry/myapp
```

**Push with digest:**
```bash
docker push myregistry/myapp:latest
# Returns: latest: digest: sha256:abc123... size: 1234

podman push myregistry/myapp:latest
```

## Pull Images

### Pull from Registry

**Pull latest:**
```bash
docker pull myregistry/myapp:latest
podman pull myregistry/myapp:latest
```

**Pull specific tag:**
```bash
docker pull myregistry/myapp:v1.0.0
podman pull myregistry/myapp:v1.0.0
```

**Pull by digest:**
```bash
docker pull myregistry/myapp@sha256:abc123...
podman pull myregistry/myapp@sha256:abc123...
```

**Pull all tags:**
```bash
docker pull --all-tags myregistry/myapp
podman pull --all-tags myregistry/myapp
```

**Pull for specific platform:**
```bash
docker pull --platform linux/arm64 myregistry/myapp:latest
podman pull --arch arm64 myregistry/myapp:latest
```

## Search Images

### Docker Hub Search

**Search Docker Hub:**
```bash
docker search nginx
docker search --filter "is-official=true" nginx
docker search --filter "stars=100" nginx

podman search nginx
podman search --filter=stars=100 nginx
```

### List Tags

**Using registry API:**
```bash
# List tags (requires curl/skopeo)
curl https://registry.hub.docker.com/v2/repositories/library/nginx/tags

# With skopeo
skopeo list-tags docker://docker.io/nginx
skopeo list-tags docker://ghcr.io/username/myapp
```

**Podman:**
```bash
podman search --list-tags docker.io/nginx
```

## Inspect Remote Images

### Without Pulling

**Inspect with skopeo:**
```bash
# Install skopeo if needed
skopeo inspect docker://myregistry/myapp:latest
skopeo inspect docker://ghcr.io/username/myapp:latest

# Show specific field
skopeo inspect docker://myapp:latest | jq '.Layers'
```

**Inspect manifest:**
```bash
# Docker
docker manifest inspect myregistry/myapp:latest

# Podman
podman manifest inspect myregistry/myapp:latest

# Skopeo
skopeo inspect --raw docker://myregistry/myapp:latest
```

## Delete Images from Registry

### Docker Hub

**Delete via web interface or API:**
```bash
# Using Docker Hub API (requires token)
curl -X DELETE \
  -H "Authorization: JWT $TOKEN" \
  https://hub.docker.com/v2/repositories/username/myapp/tags/latest/
```

### GitHub Container Registry

**Delete with GitHub CLI:**
```bash
gh api -X DELETE /user/packages/container/myapp/versions/VERSION_ID
```

### Private Registry

**Using registry API:**
```bash
# Get digest
DIGEST=$(skopeo inspect docker://myregistry/myapp:latest | jq -r '.Digest')

# Delete by digest
curl -X DELETE https://myregistry/v2/myapp/manifests/$DIGEST
```

**Podman:**
```bash
podman image rm myregistry/myapp:latest
# Note: This only removes locally, not from registry
```

## Copy Between Registries

### Using skopeo

**Copy image between registries:**
```bash
# Copy from Docker Hub to GHCR
skopeo copy \
  docker://docker.io/username/myapp:latest \
  docker://ghcr.io/username/myapp:latest

# Copy with authentication
skopeo copy \
  --src-creds username:password \
  --dest-creds username:token \
  docker://registry1.com/myapp:latest \
  docker://registry2.com/myapp:latest
```

**Copy all tags:**
```bash
skopeo sync \
  --src docker --dest docker \
  username/myapp \
  ghcr.io/username/
```

### Using Docker/Podman

**Pull, tag, push:**
```bash
docker pull registry1.com/myapp:latest
docker tag registry1.com/myapp:latest registry2.com/myapp:latest
docker push registry2.com/myapp:latest
```

## Manage Credentials

### Docker

**Credentials stored in:**
```bash
~/.docker/config.json
```

**Use credential helper:**
```bash
# Install credential helper
# For Linux: docker-credential-secretservice
# For macOS: docker-credential-osxkeychain
# For Windows: docker-credential-wincred

# Configure in ~/.docker/config.json
{
  "credsStore": "secretservice"
}
```

### Podman

**Credentials stored in:**
```bash
# Rootless
$XDG_RUNTIME_DIR/containers/auth.json

# Rootful
/run/containers/0/auth.json
```

**View stored credentials:**
```bash
cat ${XDG_RUNTIME_DIR}/containers/auth.json | jq
```

## Registry Configuration

### Docker Daemon

**Configure in /etc/docker/daemon.json:**
```json
{
  "insecure-registries": ["myregistry.local:5000"],
  "registry-mirrors": ["https://mirror.example.com"],
  "max-concurrent-downloads": 3,
  "max-concurrent-uploads": 5
}
```

### Podman

**Configure in /etc/containers/registries.conf:**
```toml
[registries.search]
registries = ['docker.io', 'ghcr.io']

[registries.insecure]
registries = ['myregistry.local:5000']

[[registry]]
location = "docker.io"
[[registry.mirror]]
location = "mirror.example.com"
```

## Run Local Registry

### Docker

**Start local registry:**
```bash
docker run -d -p 5000:5000 --name registry registry:2

# With persistence
docker run -d -p 5000:5000 \
  --name registry \
  -v registry-data:/var/lib/registry \
  registry:2

# With authentication
docker run -d -p 5000:5000 \
  --name registry \
  -v registry-data:/var/lib/registry \
  -v $(pwd)/auth:/auth \
  -e REGISTRY_AUTH=htpasswd \
  -e REGISTRY_AUTH_HTPASSWD_PATH=/auth/htpasswd \
  -e REGISTRY_AUTH_HTPASSWD_REALM="Registry Realm" \
  registry:2
```

**Use local registry:**
```bash
docker tag myapp:latest localhost:5000/myapp:latest
docker push localhost:5000/myapp:latest
```

### Podman

**Start local registry:**
```bash
podman run -d -p 5000:5000 --name registry docker.io/library/registry:2
```

## Best Practices

### Tagging Strategy

- Use semantic versioning: `v1.2.3`
- Tag with git commit SHA: `abc123`
- Tag with build date: `2024-01-15`
- Always tag `latest` for current production
- Tag stable releases: `stable`

### Security

- Use credential helpers, not plain text
- Rotate access tokens regularly
- Use least privilege access
- Scan images before pushing
- Sign images with Docker Content Trust / Cosign

### Optimization

- Use multi-stage builds to reduce size
- Push during off-peak hours for large images
- Use registry mirrors for faster pulls
- Enable layer compression

### Naming Conventions

```
registry.example.com/project/app:version
ghcr.io/username/myapp:v1.0.0
docker.io/library/nginx:1.25-alpine
```

## Common Issues

**Issue**: Authentication failed
- Check credentials
- Verify token hasn't expired
- Ensure proper permissions

**Issue**: Push denied
- Check repository permissions
- Verify namespace/organization access
- May need to create repository first

**Issue**: Image not found
- Check image name and tag
- Verify registry URL
- Ensure image was pushed successfully

**Issue**: TLS certificate error
- Add registry to insecure registries (not recommended)
- Install proper CA certificates
- Use `--tls-verify=false` for testing only

**Issue**: Rate limit exceeded (Docker Hub)
- Login to increase limit
- Use registry mirror
- Upgrade to paid plan

## Output Format

After registry operation:
```
✓ Image pushed successfully
  Runtime: docker/podman
  Image: ghcr.io/username/myapp:latest
  Registry: ghcr.io
  Size: 125 MB
  Digest: sha256:abc123...

  Tags pushed:
    - latest
    - v1.0.0
    - 2024-01-15

  Pull command:
    docker pull ghcr.io/username/myapp:latest

  Verify:
    skopeo inspect docker://ghcr.io/username/myapp:latest
```

## Quick Reference

```bash
# Login
docker login ghcr.io
podman login ghcr.io

# Tag and push
docker tag myapp:latest ghcr.io/username/myapp:latest
docker push ghcr.io/username/myapp:latest

# Pull
docker pull ghcr.io/username/myapp:latest

# Inspect remote image (skopeo)
skopeo inspect docker://ghcr.io/username/myapp:latest

# Copy between registries (skopeo)
skopeo copy docker://src/image:tag docker://dest/image:tag
```
