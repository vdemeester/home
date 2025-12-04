# Build Workflow

Building container images with Tekton using Kaniko and Buildah.

## When to Use

- Building Docker/OCI container images
- Pushing images to registries
- Implementing caching strategies
- Building without Docker daemon

## Kaniko

**Builds Docker images inside Kubernetes without Docker daemon**

### Kaniko Task from Tekton Hub

```yaml
apiVersion: tekton.dev/v1
kind: TaskRun
metadata:
  name: kaniko-build
spec:
  taskRef:
    resolver: hub
    params:
      - name: name
        value: kaniko
      - name: version
        value: "0.6.0"
  params:
    - name: IMAGE
      value: myregistry.com/myapp:latest
    - name: DOCKERFILE
      value: ./Dockerfile
    - name: CONTEXT
      value: .
    - name: EXTRA_ARGS
      value:
        - --cache=true
        - --cache-ttl=24h
  workspaces:
    - name: source
      persistentVolumeClaim:
        claimName: source-pvc
    - name: dockerconfig
      secret:
        secretName: docker-config
```

### Custom Kaniko Task

```yaml
apiVersion: tekton.dev/v1
kind: Task
metadata:
  name: kaniko-build-push
spec:
  params:
    - name: IMAGE
      description: Image name with tag
    - name: DOCKERFILE
      description: Path to Dockerfile
      default: ./Dockerfile
    - name: CONTEXT
      description: Build context
      default: .
    - name: BUILD_ARGS
      type: array
      description: Build arguments
      default: []
  workspaces:
    - name: source
      description: Source code
    - name: dockerconfig
      description: Docker credentials
  results:
    - name: IMAGE_DIGEST
      description: Image digest
  steps:
    - name: build-and-push
      image: gcr.io/kaniko-project/executor:latest
      args:
        - --dockerfile=$(params.DOCKERFILE)
        - --context=$(workspaces.source.path)/$(params.CONTEXT)
        - --destination=$(params.IMAGE)
        - --digest-file=$(results.IMAGE_DIGEST.path)
        - $(params.BUILD_ARGS[*])
      env:
        - name: DOCKER_CONFIG
          value: $(workspaces.dockerconfig.path)
      resources:
        requests:
          cpu: 1000m
          memory: 1Gi
        limits:
          cpu: 2000m
          memory: 4Gi
```

### Kaniko with Caching

```yaml
args:
  - --dockerfile=./Dockerfile
  - --context=.
  - --destination=myregistry.com/myapp:latest
  - --cache=true
  - --cache-repo=myregistry.com/cache
  - --cache-ttl=24h
  - --compressed-caching=false
```

### Multi-Stage Dockerfile with Kaniko

```dockerfile
# Dockerfile
FROM maven:3.8-openjdk-17 AS build
WORKDIR /app
COPY pom.xml .
RUN mvn dependency:go-offline
COPY src ./src
RUN mvn package -DskipTests

FROM eclipse-temurin:17-jre-alpine
COPY --from=build /app/target/*.jar /app/app.jar
ENTRYPOINT ["java", "-jar", "/app/app.jar"]
```

## Buildah

**Builds OCI images with or without Dockerfiles**

### Buildah Task

```yaml
apiVersion: tekton.dev/v1
kind: Task
metadata:
  name: buildah-build
spec:
  params:
    - name: IMAGE
      description: Image name with tag
    - name: DOCKERFILE
      description: Path to Dockerfile
      default: ./Dockerfile
    - name: CONTEXT
      description: Build context
      default: .
    - name: FORMAT
      description: Image format (oci or docker)
      default: oci
    - name: TLS_VERIFY
      description: Verify TLS certificates
      default: "true"
  workspaces:
    - name: source
    - name: dockerconfig
  results:
    - name: IMAGE_DIGEST
  steps:
    - name: build
      image: quay.io/buildah/stable:latest
      workingDir: $(workspaces.source.path)
      script: |
        #!/bin/bash
        buildah --storage-driver=vfs bud \
          --format=$(params.FORMAT) \
          --tls-verify=$(params.TLS_VERIFY) \
          --no-cache \
          -f $(params.DOCKERFILE) \
          -t $(params.IMAGE) \
          $(params.CONTEXT)

        buildah --storage-driver=vfs push \
          --tls-verify=$(params.TLS_VERIFY) \
          --digestfile=$(results.IMAGE_DIGEST.path) \
          $(params.IMAGE) \
          docker://$(params.IMAGE)
      env:
        - name: REGISTRY_AUTH_FILE
          value: $(workspaces.dockerconfig.path)/config.json
      securityContext:
        privileged: true
      resources:
        requests:
          cpu: 1000m
          memory: 1Gi
        limits:
          cpu: 2000m
          memory: 4Gi
```

### Buildah without Dockerfile

```yaml
steps:
  - name: build-from-scratch
    image: quay.io/buildah/stable
    script: |
      #!/bin/bash
      # Create container from base image
      container=$(buildah from alpine:latest)

      # Install packages
      buildah run $container -- apk add --no-cache python3

      # Copy application
      buildah copy $container app.py /app/

      # Configure
      buildah config --entrypoint '["python3", "/app/app.py"]' $container
      buildah config --port 8080 $container

      # Commit and push
      buildah commit $container $(params.IMAGE)
      buildah push $(params.IMAGE)
```

## Container Registry Authentication

### Docker Config Secret

```bash
# Create docker config secret
kubectl create secret docker-registry docker-config \
  --docker-server=myregistry.com \
  --docker-username=myuser \
  --docker-password=mypassword \
  --docker-email=myemail@example.com
```

**Or from existing config:**
```bash
kubectl create secret generic docker-config \
  --from-file=config.json=$HOME/.docker/config.json
```

### Using in Workspace

```yaml
workspaces:
  - name: dockerconfig
    secret:
      secretName: docker-config
```

### Harbor Integration

```bash
# Create secret for Harbor
kubectl create secret docker-registry harbor-config \
  --docker-server=harbor.example.com \
  --docker-username=admin \
  --docker-password=Harbor12345
```

### AWS ECR Authentication

```yaml
# Create ECR credentials task
apiVersion: tekton.dev/v1
kind: Task
metadata:
  name: ecr-login
spec:
  results:
    - name: docker-config
  steps:
    - name: get-credentials
      image: amazon/aws-cli
      script: |
        aws ecr get-login-password --region us-east-1 > $(results.docker-config.path)
```

## Complete Build Pipeline

```yaml
apiVersion: tekton.dev/v1
kind: Pipeline
metadata:
  name: build-push-pipeline
spec:
  params:
    - name: git-url
    - name: git-revision
      default: main
    - name: image-name
    - name: dockerfile
      default: ./Dockerfile
  workspaces:
    - name: source-code
    - name: docker-credentials
  tasks:
    # Clone repository
    - name: clone
      taskRef:
        resolver: hub
        params:
          - name: name
            value: git-clone
          - name: version
            value: "0.10.0"
      workspaces:
        - name: output
          workspace: source-code
      params:
        - name: url
          value: $(params.git-url)
        - name: revision
          value: $(params.git-revision)

    # Extract version from git
    - name: get-version
      runAfter:
        - clone
      taskRef:
        name: git-version
      workspaces:
        - name: source
          workspace: source-code

    # Build and push with Kaniko
    - name: build-push
      runAfter:
        - get-version
      taskRef:
        resolver: hub
        params:
          - name: name
            value: kaniko
      workspaces:
        - name: source
          workspace: source-code
        - name: dockerconfig
          workspace: docker-credentials
      params:
        - name: IMAGE
          value: $(params.image-name):$(tasks.get-version.results.version)
        - name: DOCKERFILE
          value: $(params.dockerfile)
        - name: EXTRA_ARGS
          value:
            - --cache=true
            - --compressed-caching=false

    # Scan image for vulnerabilities
    - name: scan
      runAfter:
        - build-push
      taskRef:
        name: trivy-scanner
      params:
        - name: IMAGE
          value: $(params.image-name):$(tasks.get-version.results.version)
        - name: DIGEST
          value: $(tasks.build-push.results.IMAGE_DIGEST)
```

## Best Practices

### Image Tagging
- Use semantic versioning: `v1.2.3`
- Include git commit SHA: `abc123`
- Tag with branch name for development: `main`, `develop`
- Always tag `latest` for main branch
- Use immutable tags in production

### Build Optimization
- Use multi-stage Dockerfiles
- Leverage build cache (Kaniko `--cache`)
- Order Dockerfile commands from least to most frequently changed
- Use `.dockerignore` to exclude unnecessary files
- Minimize layer count

### Security
- Scan images for vulnerabilities (Trivy, Grype)
- Use minimal base images (alpine, distroless)
- Run as non-root user
- Don't include secrets in images
- Sign images (Cosign)

### Resource Management
- Set appropriate CPU/memory limits
- Use caching to reduce build time
- Clean up intermediate images
- Limit concurrent builds

## Troubleshooting

### Kaniko Build Fails

```bash
# Check workspace is populated
kubectl exec <pod-name> -c step-build-and-push -- ls /workspace/source

# Check Dockerfile path
kubectl exec <pod-name> -c step-build-and-push -- cat /workspace/source/Dockerfile

# Check registry credentials
kubectl get secret docker-config -o yaml

# View detailed logs
tkn taskrun logs <taskrun-name> -f
```

### Authentication Errors

```bash
# Verify secret format
kubectl get secret docker-config -o jsonpath='{.data.config\.json}' | base64 -d | jq

# Test registry access
kubectl run test-registry --rm -i --tty --image=alpine -- sh
apk add --no-cache docker
docker login myregistry.com
```

### Image Push Fails

```bash
# Check network connectivity
kubectl exec <pod-name> -- ping myregistry.com

# Verify registry URL
kubectl exec <pod-name> -- nslookup myregistry.com

# Check TLS verification
# Add --insecure flag for testing (not production!)
```

### Out of Memory

```bash
# Increase memory limits
resources:
  limits:
    memory: 8Gi

# Use Kaniko compressed caching
- --compressed-caching=true

# Reduce concurrent builds
```
