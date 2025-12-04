# Tasks Workflow

Creating and managing Tekton Tasks - the fundamental building blocks of Tekton Pipelines.

## When to Use

- Creating new Tasks
- Defining Steps and workspaces
- Parameterizing tasks for reusability
- Emitting Results
- Understanding TaskRuns

## Task Anatomy

### Basic Task Structure

```yaml
apiVersion: tekton.dev/v1
kind: Task
metadata:
  name: my-task
  namespace: default
spec:
  description: |
    A brief description of what this task does
  params:
    - name: param-name
      type: string
      description: Description of the parameter
      default: "default-value"
  workspaces:
    - name: workspace-name
      description: Workspace for source code
      mountPath: /workspace/source
  results:
    - name: result-name
      description: Description of the result
  steps:
    - name: step-name
      image: alpine:latest
      script: |
        #!/bin/sh
        echo "Hello from step"
```

### Complete Task Example

```yaml
apiVersion: tekton.dev/v1
kind: Task
metadata:
  name: build-app
  labels:
    app.kubernetes.io/version: "1.0"
spec:
  description: Build application using Maven
  params:
    - name: maven-image
      type: string
      description: Maven image to use
      default: maven:3.8-openjdk-17
    - name: goals
      type: array
      description: Maven goals to execute
      default:
        - clean
        - package
    - name: settings-path
      type: string
      description: Path to Maven settings.xml
      default: ""
  workspaces:
    - name: source
      description: Source code workspace
    - name: maven-cache
      description: Maven local repository cache
      optional: true
  results:
    - name: artifact-name
      description: Name of the built artifact
    - name: build-timestamp
      description: When the build completed
  steps:
    - name: build
      image: $(params.maven-image)
      workingDir: $(workspaces.source.path)
      script: |
        #!/bin/bash
        set -e

        # Use cache if available
        if [ "$(workspaces.maven-cache.bound)" == "true" ]; then
          export MAVEN_OPTS="-Dmaven.repo.local=$(workspaces.maven-cache.path)"
        fi

        # Add settings if provided
        SETTINGS_ARG=""
        if [ -n "$(params.settings-path)" ]; then
          SETTINGS_ARG="-s $(params.settings-path)"
        fi

        # Run Maven build
        mvn $SETTINGS_ARG $(params.goals[*])

        # Output results
        ARTIFACT=$(ls target/*.jar | head -n 1 | xargs basename)
        echo -n "$ARTIFACT" > $(results.artifact-name.path)
        echo -n "$(date -u +%Y-%m-%dT%H:%M:%SZ)" > $(results.build-timestamp.path)
      resources:
        requests:
          cpu: 500m
          memory: 1Gi
        limits:
          cpu: 2000m
          memory: 4Gi
```

## Parameters

### Parameter Types

**String parameter:**
```yaml
params:
  - name: git-url
    type: string
    description: Git repository URL
    default: ""
```

**Array parameter:**
```yaml
params:
  - name: build-args
    type: array
    description: Build arguments
    default: []
```

**Object parameter (Beta):**
```yaml
params:
  - name: gitconfig
    type: object
    description: Git configuration
    properties:
      username:
        type: string
      email:
        type: string
    default:
      username: ""
      email: ""
```

### Using Parameters in Steps

```yaml
steps:
  - name: use-params
    image: alpine
    script: |
      echo "String param: $(params.git-url)"
      echo "Array param: $(params.build-args[*])"
      echo "Array first element: $(params.build-args[0])"
      echo "Object param: $(params.gitconfig.username)"
```

### Parameter Validation

```yaml
params:
  - name: environment
    type: string
    description: Deployment environment
    default: "dev"
    # Validation happens at runtime via when expressions or script logic
```

## Workspaces

### Declaring Workspaces

```yaml
workspaces:
  - name: source
    description: Source code directory
    mountPath: /workspace/source  # Optional custom mount path
  - name: cache
    description: Build cache
    optional: true  # Task can run without this workspace
  - name: config
    description: Configuration files
    readOnly: true  # Workspace is read-only
```

### Workspace Variables

```yaml
script: |
  # Path to workspace
  echo "Workspace path: $(workspaces.source.path)"

  # PVC name (if using PVC)
  echo "PVC name: $(workspaces.source.claim)"

  # Check if workspace is bound
  if [ "$(workspaces.source.bound)" == "true" ]; then
    echo "Workspace is available"
  fi
```

### Workspace Use Cases

**PVC workspace (shared data):**
```yaml
# In TaskRun
workspaces:
  - name: source
    persistentVolumeClaim:
      claimName: shared-workspace
```

**emptyDir workspace (temporary):**
```yaml
# In TaskRun
workspaces:
  - name: temp
    emptyDir: {}
```

**ConfigMap workspace (configuration):**
```yaml
# In TaskRun
workspaces:
  - name: config
    configMap:
      name: app-config
```

**Secret workspace (credentials):**
```yaml
# In TaskRun
workspaces:
  - name: credentials
    secret:
      secretName: git-credentials
```

## Steps

### Step Structure

```yaml
steps:
  - name: step-name
    image: container-image
    command:
      - /bin/sh
    args:
      - -c
      - echo "Hello"
    env:
      - name: ENV_VAR
        value: "value"
    workingDir: /workspace
    script: |
      #!/bin/sh
      echo "Multi-line script"
    resources:
      requests:
        cpu: 100m
        memory: 128Mi
      limits:
        cpu: 500m
        memory: 512Mi
    securityContext:
      runAsNonRoot: true
      runAsUser: 1000
```

### Multiple Steps (Sequential)

```yaml
steps:
  - name: fetch-dependencies
    image: node:18
    script: |
      npm install

  - name: run-tests
    image: node:18
    script: |
      npm test

  - name: build
    image: node:18
    script: |
      npm run build
```

### Step Script vs Command

**Using script (recommended):**
```yaml
steps:
  - name: build
    image: maven:3.8
    script: |
      #!/bin/bash
      set -e
      mvn clean package
```

**Using command/args:**
```yaml
steps:
  - name: build
    image: maven:3.8
    command:
      - mvn
    args:
      - clean
      - package
```

### Environment Variables

```yaml
steps:
  - name: build
    image: alpine
    env:
      # From parameter
      - name: GIT_URL
        value: $(params.git-url)

      # From secret
      - name: API_KEY
        valueFrom:
          secretKeyRef:
            name: api-credentials
            key: api_key

      # From configMap
      - name: CONFIG_PATH
        valueFrom:
          configMapKeyRef:
            name: app-config
            key: config_path

      # Built-in Tekton variables
      - name: TASK_NAME
        value: $(context.task.name)
      - name: TASKRUN_NAME
        value: $(context.taskRun.name)
      - name: TASKRUN_NAMESPACE
        value: $(context.taskRun.namespace)
```

## Results

### Emitting Results

**IMPORTANT:** Results are limited to 4KB. Use workspaces for larger data.

```yaml
results:
  - name: commit-sha
    description: Git commit SHA
  - name: image-digest
    description: Container image digest
  - name: test-status
    description: Test pass/fail status

steps:
  - name: get-commit
    image: alpine/git
    script: |
      git rev-parse HEAD > $(results.commit-sha.path)

  - name: build-image
    image: gcr.io/kaniko-project/executor
    script: |
      # Kaniko outputs digest to /kaniko/digest
      cp /kaniko/digest $(results.image-digest.path)

  - name: run-tests
    image: node:18
    script: |
      if npm test; then
        echo -n "passed" > $(results.test-status.path)
      else
        echo -n "failed" > $(results.test-status.path)
      fi
```

### Result Size Considerations

**Good (small data):**
- Commit SHAs
- Image digests
- Version numbers
- Status flags
- Ephemeral namespace names

**Bad (use workspaces instead):**
- Full build logs
- Large JSON payloads
- File contents
- Binary data

## Resource Management

### Step Resources

```yaml
steps:
  - name: build
    image: maven:3.8
    resources:
      requests:
        cpu: 500m
        memory: 1Gi
      limits:
        cpu: 2000m
        memory: 4Gi
```

### Task-Level Resources (Alpha)

```yaml
# In TaskRun, not Task definition
apiVersion: tekton.dev/v1beta1
kind: TaskRun
spec:
  taskRef:
    name: build-task
  computeResources:
    requests:
      cpu: "1"
      memory: "2Gi"
    limits:
      cpu: "2"
      memory: "4Gi"
```

**Best Practices:**
- Always set resource requests for scheduler
- Set limits 1.5-2x higher than requests
- Monitor actual usage with `kubectl top pods`
- Use task-level resources for simplicity when all steps have similar requirements

## Security Context

### Task Security

```yaml
apiVersion: tekton.dev/v1
kind: Task
metadata:
  name: secure-task
spec:
  steps:
    - name: build
      image: alpine
      securityContext:
        runAsNonRoot: true
        runAsUser: 1000
        runAsGroup: 1000
        allowPrivilegeEscalation: false
        readOnlyRootFilesystem: true
        capabilities:
          drop:
            - ALL
      volumeMounts:
        - name: tmp
          mountPath: /tmp
  volumes:
    - name: tmp
      emptyDir: {}
```

## TaskRun

### Creating TaskRun

**Imperative (tkn):**
```bash
tkn task start my-task \
  -p param1=value1 \
  -p param2=value2 \
  -w name=source,claimName=my-pvc \
  --showlog
```

**Declarative (YAML):**
```yaml
apiVersion: tekton.dev/v1
kind: TaskRun
metadata:
  name: my-taskrun
spec:
  taskRef:
    name: my-task
  params:
    - name: param1
      value: "value1"
    - name: param2
      value: "value2"
  workspaces:
    - name: source
      persistentVolumeClaim:
        claimName: my-pvc
  serviceAccountName: pipeline-sa
  timeout: 1h
```

### TaskRun with Resolvers

**Hub resolver:**
```yaml
apiVersion: tekton.dev/v1
kind: TaskRun
metadata:
  name: git-clone-run
spec:
  taskRef:
    resolver: hub
    params:
      - name: name
        value: git-clone
      - name: version
        value: "0.10.0"
  params:
    - name: url
      value: https://github.com/myorg/myrepo
  workspaces:
    - name: output
      emptyDir: {}
```

**Cluster resolver:**
```yaml
apiVersion: tekton.dev/v1
kind: TaskRun
metadata:
  name: shared-task-run
spec:
  taskRef:
    resolver: cluster
    params:
      - name: name
        value: shared-task
      - name: namespace
        value: tekton-tasks
```

### TaskRun Status

```bash
# View taskrun status
kubectl get taskrun my-taskrun

# Describe for details
kubectl describe taskrun my-taskrun

# Get status conditions
kubectl get taskrun my-taskrun -o jsonpath='{.status.conditions}'

# Get results
kubectl get taskrun my-taskrun -o jsonpath='{.status.taskResults}'
```

## Reusable Task Patterns

### Git Clone Task

```yaml
apiVersion: tekton.dev/v1
kind: Task
metadata:
  name: git-clone-custom
spec:
  params:
    - name: url
      description: Git repository URL
    - name: revision
      description: Branch, tag, or commit
      default: "main"
    - name: depth
      description: Shallow clone depth
      default: "1"
  workspaces:
    - name: output
      description: Clone destination
  steps:
    - name: clone
      image: alpine/git:latest
      script: |
        git clone --depth=$(params.depth) \
          --branch=$(params.revision) \
          $(params.url) \
          $(workspaces.output.path)
```

### Test Runner Task

```yaml
apiVersion: tekton.dev/v1
kind: Task
metadata:
  name: run-tests
spec:
  params:
    - name: test-command
      description: Command to run tests
      default: "npm test"
  workspaces:
    - name: source
      description: Source code
  results:
    - name: test-result
      description: Test pass/fail
  steps:
    - name: test
      image: node:18
      workingDir: $(workspaces.source.path)
      script: |
        if $(params.test-command); then
          echo -n "passed" > $(results.test-result.path)
        else
          echo -n "failed" > $(results.test-result.path)
          exit 1
        fi
```

## Best Practices

### Task Design
- Keep tasks focused on single responsibility
- Use parameters for configurability (no hardcoded values)
- Document all parameters, workspaces, and results
- Emit results for small data (<4KB)
- Use workspaces for sharing larger data
- Set resource requests/limits for all steps
- Use non-root containers when possible

### Reusability
- Check Tekton Hub before creating custom tasks
- Make tasks generic and parameterized
- Use descriptive names and labels
- Version your tasks (labels/annotations)
- Write comprehensive descriptions

### Security
- Run as non-root user
- Use read-only root filesystem when possible
- Drop unnecessary capabilities
- Use workspace-based authentication
- Avoid embedding secrets in task definitions

### Performance
- Use caching workspaces (Maven, npm, Go)
- Set appropriate resource limits
- Use specific image tags (not :latest)
- Minimize image sizes (alpine, distroless)
- Use build caches (Kaniko, Buildah)

## Common Patterns

### Conditional Execution

```yaml
steps:
  - name: deploy-to-prod
    image: kubectl
    script: |
      if [ "$(params.environment)" = "production" ]; then
        kubectl apply -f production.yaml
      else
        echo "Skipping production deployment"
      fi
```

### Error Handling

```yaml
steps:
  - name: build-with-retry
    image: maven:3.8
    script: |
      #!/bin/bash
      set -e

      MAX_RETRIES=3
      RETRY_COUNT=0

      until mvn clean package; do
        RETRY_COUNT=$((RETRY_COUNT+1))
        if [ $RETRY_COUNT -eq $MAX_RETRIES ]; then
          echo "Build failed after $MAX_RETRIES attempts"
          exit 1
        fi
        echo "Build failed, retrying ($RETRY_COUNT/$MAX_RETRIES)..."
        sleep 5
      done
```

### Using Previous Step Results

```yaml
steps:
  - name: get-version
    image: alpine
    script: |
      echo -n "1.2.3" > /tmp/version

  - name: use-version
    image: alpine
    script: |
      VERSION=$(cat /tmp/version)
      echo "Building version: $VERSION"
```

## Troubleshooting

### Task Won't Start

```bash
# Check task exists
kubectl get task my-task

# Check taskrun status
kubectl describe taskrun my-taskrun

# Check events
kubectl get events --sort-by=.metadata.creationTimestamp
```

### Step Failing

```bash
# View logs
tkn taskrun logs my-taskrun -f

# Check specific step
kubectl logs <pod-name> -c step-<step-name>

# Check previous container logs if crashed
kubectl logs <pod-name> -c step-<step-name> --previous
```

### Workspace Issues

```bash
# Check PVC exists and is bound
kubectl get pvc

# Check workspace binding in taskrun
kubectl get taskrun my-taskrun -o jsonpath='{.spec.workspaces}'

# Check pod volumes
kubectl get pod <pod-name> -o jsonpath='{.spec.volumes}'
```

### Results Not Available

```bash
# Check result was written
kubectl get taskrun my-taskrun -o jsonpath='{.status.taskResults}'

# Check step completed successfully
kubectl get taskrun my-taskrun -o jsonpath='{.status.steps}'

# Verify result size (<4KB)
kubectl logs <pod-name> -c step-<step-name> | wc -c
```
