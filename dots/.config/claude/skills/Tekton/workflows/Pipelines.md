# Pipelines Workflow

Creating and managing Tekton Pipelines to orchestrate multiple Tasks into complete CI/CD workflows.

## When to Use

- Orchestrating multiple tasks
- Defining task dependencies and execution order
- Creating complete CI/CD workflows
- Managing data flow between tasks
- Building complex pipelines

## Pipeline Structure

### Basic Pipeline

```yaml
apiVersion: tekton.dev/v1
kind: Pipeline
metadata:
  name: build-and-deploy
spec:
  params:
    - name: git-url
      type: string
    - name: image-name
      type: string
  workspaces:
    - name: shared-data
  tasks:
    - name: fetch-source
      taskRef:
        name: git-clone
      workspaces:
        - name: output
          workspace: shared-data
      params:
        - name: url
          value: $(params.git-url)

    - name: build-image
      taskRef:
        name: kaniko
      runAfter:
        - fetch-source
      workspaces:
        - name: source
          workspace: shared-data
      params:
        - name: IMAGE
          value: $(params.image-name)
```

## Task Execution Control

### Sequential Execution

```yaml
tasks:
  - name: test
    taskRef:
      name: run-tests

  - name: build
    taskRef:
      name: build-image
    runAfter:
      - test  # Waits for test to complete
```

### Parallel Execution

```yaml
tasks:
  - name: lint
    taskRef:
      name: lint-code
    # No runAfter - starts immediately

  - name: test
    taskRef:
      name: run-tests
    # No runAfter - starts in parallel with lint

  - name: security-scan
    taskRef:
      name: trivy-scan
    # No runAfter - starts in parallel

  - name: build
    taskRef:
      name: build-image
    runAfter:
      - lint
      - test
      - security-scan  # Waits for all three
```

### Conditional Execution

```yaml
tasks:
  - name: deploy-staging
    when:
      - input: "$(params.environment)"
        operator: in
        values: ["staging", "production"]
    taskRef:
      name: deploy

  - name: deploy-prod
    when:
      - input: "$(params.environment)"
        operator: in
        values: ["production"]
      - input: "$(tasks.test.results.status)"
        operator: in
        values: ["passed"]
    runAfter:
      - deploy-staging
    taskRef:
      name: deploy-production
```

**When Expression Operators:**
- `in`: Value is in list
- `notin`: Value is not in list

## Passing Data Between Tasks

### Using Results

```yaml
tasks:
  - name: get-version
    taskRef:
      name: calculate-version
    # Task emits result: version

  - name: build-image
    taskRef:
      name: kaniko
    runAfter:
      - get-version
    params:
      - name: IMAGE
        value: $(params.image-name):$(tasks.get-version.results.version)
```

### Using Workspaces

```yaml
workspaces:
  - name: source-code

tasks:
  - name: clone
    taskRef:
      name: git-clone
    workspaces:
      - name: output
        workspace: source-code

  - name: test
    taskRef:
      name: run-tests
    runAfter:
      - clone
    workspaces:
      - name: source
        workspace: source-code  # Shares data with clone task
```

## Finally Tasks

**Finally tasks run after all pipeline tasks complete, regardless of success or failure.**

```yaml
finally:
  - name: cleanup-workspace
    taskRef:
      name: cleanup
    workspaces:
      - name: workspace
        workspace: shared-data

  - name: send-notification
    taskRef:
      name: send-slack-message
    params:
      - name: status
        value: $(tasks.status)  # SUCCESS or FAILURE
      - name: message
        value: "Pipeline $(context.pipelineRun.name) completed"
```

## Complete Pipeline Example

```yaml
apiVersion: tekton.dev/v1
kind: Pipeline
metadata:
  name: nodejs-ci-cd
spec:
  params:
    - name: git-url
      description: Git repository URL
    - name: git-revision
      description: Git revision
      default: main
    - name: image-name
      description: Container image name
    - name: deploy-namespace
      description: Kubernetes namespace
      default: default

  workspaces:
    - name: source-code
    - name: docker-credentials

  tasks:
    # Fetch source code
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

    # Parallel: Lint and test
    - name: lint
      taskRef:
        name: npm-lint
      runAfter:
        - clone
      workspaces:
        - name: source
          workspace: source-code

    - name: test
      taskRef:
        name: npm-test
      runAfter:
        - clone
      workspaces:
        - name: source
          workspace: source-code

    # Security scan
    - name: security-scan
      taskRef:
        name: trivy-scan
      runAfter:
        - clone
      workspaces:
        - name: source
          workspace: source-code

    # Build and push image
    - name: build-push
      taskRef:
        resolver: hub
        params:
          - name: name
            value: kaniko
          - name: version
            value: "0.6.0"
      runAfter:
        - lint
        - test
        - security-scan
      workspaces:
        - name: source
          workspace: source-code
        - name: dockerconfig
          workspace: docker-credentials
      params:
        - name: IMAGE
          value: $(params.image-name):$(tasks.clone.results.commit)

    # Deploy to Kubernetes
    - name: deploy
      taskRef:
        name: kubernetes-deploy
      runAfter:
        - build-push
      params:
        - name: image
          value: $(params.image-name):$(tasks.clone.results.commit)
        - name: namespace
          value: $(params.deploy-namespace)

  finally:
    - name: cleanup
      taskRef:
        name: cleanup-workspace
      workspaces:
        - name: workspace
          workspace: source-code

    - name: notify
      taskRef:
        name: send-notification
      params:
        - name: pipeline-status
          value: $(tasks.status)
```

## PipelineRun

### Creating PipelineRun

**Using tkn CLI:**
```bash
tkn pipeline start nodejs-ci-cd \
  -p git-url=https://github.com/myorg/myapp \
  -p git-revision=main \
  -p image-name=myregistry.com/myapp \
  -p deploy-namespace=production \
  -w name=source-code,volumeClaimTemplateFile=pvc-template.yaml \
  -w name=docker-credentials,secret=docker-config \
  --serviceaccount=pipeline-sa \
  --showlog
```

**Declarative YAML:**
```yaml
apiVersion: tekton.dev/v1
kind: PipelineRun
metadata:
  generateName: nodejs-ci-cd-run-
spec:
  pipelineRef:
    name: nodejs-ci-cd
  params:
    - name: git-url
      value: https://github.com/myorg/myapp
    - name: git-revision
      value: main
    - name: image-name
      value: myregistry.com/myapp
    - name: deploy-namespace
      value: production
  workspaces:
    - name: source-code
      volumeClaimTemplate:
        spec:
          accessModes:
            - ReadWriteOnce
          resources:
            requests:
              storage: 1Gi
    - name: docker-credentials
      secret:
        secretName: docker-config
  serviceAccountName: pipeline-sa
  timeout: 1h
```

## Advanced Patterns

### Matrix Execution (Beta)

**Run task with multiple parameter combinations:**
```yaml
tasks:
  - name: test-platforms
    taskRef:
      name: run-tests
    matrix:
      params:
        - name: platform
          value:
            - linux/amd64
            - linux/arm64
        - name: version
          value:
            - "18"
            - "20"
    # Creates 4 TaskRuns: all combinations of platform x version
```

### Custom Task References

```yaml
tasks:
  - name: custom-action
    taskRef:
      apiVersion: custom.dev/v1
      kind: CustomTask
      name: my-custom-task
```

### Retries

```yaml
tasks:
  - name: flaky-test
    retries: 3
    taskRef:
      name: integration-tests
```

### Timeouts

```yaml
# Pipeline-level timeout
spec:
  timeouts:
    pipeline: 2h
    tasks: 1h
    finally: 30m

# Task-level timeout
tasks:
  - name: long-running
    timeout: 45m
    taskRef:
      name: build-task
```

### OnError Behavior

```yaml
tasks:
  - name: optional-scan
    onError: continue  # Continue even if this task fails
    taskRef:
      name: security-scan
```

## Common Pipeline Patterns

### Build-Test-Deploy

```yaml
spec:
  tasks:
    - name: fetch-repo
    - name: run-tests
      runAfter: [fetch-repo]
    - name: build-image
      runAfter: [run-tests]
    - name: deploy
      runAfter: [build-image]
```

### Fan-Out, Fan-In

```yaml
spec:
  tasks:
    - name: clone

    # Fan out (parallel)
    - name: unit-test
      runAfter: [clone]
    - name: integration-test
      runAfter: [clone]
    - name: e2e-test
      runAfter: [clone]

    # Fan in (wait for all)
    - name: build
      runAfter: [unit-test, integration-test, e2e-test]
```

### Multi-Environment Deploy

```yaml
spec:
  params:
    - name: environment
  tasks:
    - name: build

    - name: deploy-dev
      when:
        - input: $(params.environment)
          operator: in
          values: [dev, staging, production]
      runAfter: [build]

    - name: deploy-staging
      when:
        - input: $(params.environment)
          operator: in
          values: [staging, production]
      runAfter: [deploy-dev]

    - name: deploy-production
      when:
        - input: $(params.environment)
          operator: in
          values: [production]
      runAfter: [deploy-staging]
```

## Monitoring and Debugging

### View Pipeline Status

```bash
# List pipelineruns
tkn pipelinerun list

# Describe pipelinerun
tkn pipelinerun describe my-run

# View logs
tkn pipelinerun logs my-run -f

# Check task status
kubectl get pipelinerun my-run -o jsonpath='{.status.taskRuns}'
```

### Pipeline Metrics

```bash
# Get pipeline duration
kubectl get pipelinerun my-run -o jsonpath='{.status.startTime}'
kubectl get pipelinerun my-run -o jsonpath='{.status.completionTime}'

# Get task durations
kubectl get pipelinerun my-run -o json | \
  jq '.status.taskRuns[] | {name: .pipelineTaskName, duration: (.status.completionTime - .status.startTime)}'
```

## Best Practices

### Pipeline Design
- Keep pipelines focused on single workflow (CI or CD, not both)
- Use descriptive task names
- Group related tasks with proper dependencies
- Use finally tasks for cleanup and notifications
- Set appropriate timeouts

### Data Flow
- Use Results for small data (<4KB): commits, versions, flags
- Use Workspaces for larger data: source code, build artifacts
- Use PVC with volumeClaimTemplate for isolation
- Be aware of PVC access mode limitations for parallel tasks

### Error Handling
- Set retries for flaky network operations
- Use onError: continue for optional tasks
- Use finally tasks for cleanup
- Implement proper logging in tasks

### Performance
- Run independent tasks in parallel
- Use caching workspaces (Maven, npm, Go modules)
- Set resource limits to prevent resource contention
- Use pipeline pruner to clean old runs

### Security
- Use dedicated ServiceAccounts
- Follow least privilege for RBAC
- Use Secrets for credentials
- Never hardcode sensitive data in pipelines

## Troubleshooting

### Pipeline Won't Start

```bash
# Check pipeline exists
kubectl get pipeline my-pipeline

# Check pipelinerun
kubectl describe pipelinerun my-run

# Check service account
kubectl get serviceaccount pipeline-sa

# Check RBAC
kubectl auth can-i create pipelineruns
```

### Task Dependencies Not Working

```bash
# Check runAfter is correct
kubectl get pipeline my-pipeline -o yaml | grep -A 5 runAfter

# Check when expressions
kubectl get pipelinerun my-run -o jsonpath='{.status.skippedTasks}'
```

### Workspace Issues

```bash
# Check workspace bindings
kubectl get pipelinerun my-run -o jsonpath='{.spec.workspaces}'

# Check PVC exists
kubectl get pvc

# Check PVC is bound
kubectl get pvc my-pvc -o jsonpath='{.status.phase}'
```

### Finally Tasks Not Running

```bash
# Check pipeline status
kubectl get pipelinerun my-run -o jsonpath='{.status.conditions}'

# View finally task status
kubectl get pipelinerun my-run -o jsonpath='{.status.finallyStatus}'
```
