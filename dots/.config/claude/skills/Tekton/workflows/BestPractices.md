# Best Practices Workflow

Tekton best practices for security, performance, reliability, and maintainability.

## When to Use

- Optimizing Tekton pipelines
- Implementing security best practices
- Improving performance
- Operating Tekton at scale
- Troubleshooting common issues

## Security Best Practices

### Workspace-Based Authentication (2024 Recommendation)

**DO: Use workspace-based authentication**
```yaml
workspaces:
  - name: git-credentials
    secret:
      secretName: git-ssh-key
```

**DON'T: Rely only on built-in credential initialization**
- Being phased out
- All steps get access to all credentials
- Credentials copied to disk
- Security issues with different UIDs

### ServiceAccount and RBAC

**Create dedicated ServiceAccounts:**
```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: pipeline-sa
secrets:
  - name: docker-credentials
  - name: git-credentials
---
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: pipeline-role
rules:
  - apiGroups: [""]
    resources: ["pods", "services"]
    verbs: ["get", "list"]
  - apiGroups: ["apps"]
    resources: ["deployments"]
    verbs: ["get", "list", "update", "patch"]
---
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: pipeline-binding
subjects:
  - kind: ServiceAccount
    name: pipeline-sa
roleRef:
  kind: Role
  name: pipeline-role
  apiGroup: rbac.authorization.k8s.io
```

**Principle of least privilege:**
- Create separate ServiceAccounts for CI and CD
- Grant only necessary permissions
- Use Role instead of ClusterRole when possible
- Regularly audit permissions

### Pod Security

```yaml
spec:
  steps:
    - name: secure-step
      image: alpine
      securityContext:
        runAsNonRoot: true
        runAsUser: 1000
        allowPrivilegeEscalation: false
        readOnlyRootFilesystem: true
        capabilities:
          drop:
            - ALL
```

### Secrets Management

**DO:**
- Use Kubernetes Secrets or external secret managers (Vault, AWS Secrets Manager)
- Mount secrets as volumes, not environment variables
- Use short-lived tokens (OAuth, OIDC)
- Rotate secrets regularly
- Scan secrets with tools like gitleaks, trufflehog

**DON'T:**
- Commit secrets to Git
- Print secrets in logs
- Use long-lived credentials
- Share secrets across environments

## Performance Best Practices

### Resource Management

**Always set resource requests and limits:**
```yaml
steps:
  - name: build
    image: maven:3.8
    resources:
      requests:
        cpu: 1000m
        memory: 2Gi
      limits:
        cpu: 2000m
        memory: 4Gi
```

**Guidelines:**
- Set requests based on average usage
- Set limits 1.5-2x higher than requests
- Monitor actual usage: `kubectl top pods`
- Use task-level resources for simplicity

### Configure Pipeline Pruner

**Automatic cleanup of old resources:**
```yaml
apiVersion: operator.tekton.dev/v1alpha1
kind: TektonConfig
metadata:
  name: config
spec:
  pruner:
    disabled: false
    schedule: "0 8 * * *"  # Daily at 8 AM
    keep: 3  # Keep last 3 runs
    resources:
      - pipelinerun
      - taskrun
```

**Manual cleanup:**
```bash
# Keep last 5 runs
tkn pipelinerun delete --keep 5 -f

# Delete runs older than 60 minutes
tkn pipelinerun delete --keep-since 60 -f

# Delete all failed runs
tkn pipelinerun delete --all --failed -f
```

### Workspace Optimization

**Use volumeClaimTemplate for isolation:**
```yaml
workspaces:
  - name: workspace
    volumeClaimTemplate:
      spec:
        accessModes:
          - ReadWriteOnce
        resources:
          requests:
            storage: 1Gi
```

**Avoid PVC sharing conflicts:**
- ReadWriteOnce: Limited to single node
- ReadWriteMany: Best for Tekton but less common
- Use emptyDir for temporary data
- Use subPaths for organization

### Caching Strategies

**Maven cache:**
```yaml
workspaces:
  - name: maven-cache
    persistentVolumeClaim:
      claimName: maven-cache-pvc

steps:
  - name: build
    image: maven:3.8
    env:
      - name: MAVEN_OPTS
        value: "-Dmaven.repo.local=$(workspaces.maven-cache.path)"
```

**Kaniko cache:**
```yaml
args:
  - --cache=true
  - --cache-repo=myregistry.com/cache
  - --cache-ttl=24h
```

**npm cache:**
```yaml
workspaces:
  - name: npm-cache

steps:
  - name: install
    image: node:18
    script: |
      npm config set cache $(workspaces.npm-cache.path)
      npm install
```

## Reliability Best Practices

### Error Handling

**Use retries for flaky operations:**
```yaml
tasks:
  - name: flaky-network-test
    retries: 3
    taskRef:
      name: integration-test
```

**Use onError for optional tasks:**
```yaml
tasks:
  - name: optional-scan
    onError: continue
    taskRef:
      name: security-scan
```

**Implement timeout:**
```yaml
spec:
  timeouts:
    pipeline: 2h
    tasks: 1h
    finally: 30m

tasks:
  - name: long-running
    timeout: 45m
```

### Finally Tasks for Cleanup

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
      name: notify
    params:
      - name: status
        value: $(tasks.status)
      - name: message
        value: "Pipeline $(context.pipelineRun.name) $(tasks.status)"
```

### Idempotent Tasks

```yaml
# BAD: Not idempotent
script: |
  echo "data" >> file.txt  # Appends on retry

# GOOD: Idempotent
script: |
  echo "data" > file.txt   # Overwrites on retry
```

## Maintainability Best Practices

### Task Reusability

**DO: Parameterize everything**
```yaml
params:
  - name: image
  - name: dockerfile
    default: ./Dockerfile
  - name: context
    default: .
```

**DON'T: Hardcode values**
```yaml
# Anti-pattern
args:
  - --destination=myregistry.com/myimage:latest
```

### Use Tekton Hub

**Check Hub before creating custom tasks:**
```bash
tkn hub search git
tkn hub info task git-clone
tkn hub install task git-clone
```

**Reference Hub tasks:**
```yaml
taskRef:
  resolver: hub
  params:
    - name: name
      value: git-clone
    - name: version
      value: "0.10.0"
```

### Migration from Deprecated Features

**AVOID: ClusterTasks (deprecated)**
```yaml
# Old
taskRef:
  name: git-clone
  kind: ClusterTask
```

**USE: Resolvers**
```yaml
# New - Cluster resolver
taskRef:
  resolver: cluster
  params:
    - name: name
      value: git-clone
    - name: namespace
      value: tekton-tasks

# New - Hub resolver (preferred)
taskRef:
  resolver: hub
  params:
    - name: name
      value: git-clone
```

**AVOID: PipelineResources (deprecated)**
- Use Tasks with Results and Workspaces instead

### Documentation

**Document tasks comprehensively:**
```yaml
metadata:
  name: build-app
  labels:
    app.kubernetes.io/version: "1.0"
  annotations:
    tekton.dev/pipelines.minVersion: "0.50.0"
    tekton.dev/categories: Build Tools
    tekton.dev/tags: build, maven
spec:
  description: |
    Builds Java application using Maven.

    This task compiles source code, runs tests,
    and packages the application as a JAR file.

    ## Parameters
    - maven-goals: Maven goals to execute (default: clean package)
    - settings-path: Path to custom settings.xml

    ## Workspaces
    - source: Source code directory
    - maven-cache: Maven local repository cache (optional)

    ## Results
    - artifact-name: Name of the generated JAR file
```

### Versioning

```yaml
metadata:
  labels:
    app.kubernetes.io/version: "2.1.0"
    version: "2.1.0"
```

## Anti-Patterns to Avoid

### 1. Not Setting Resource Limits

**Impact:** Poor scheduling, resource contention, cluster instability

**Solution:**
```yaml
resources:
  requests:
    cpu: 500m
    memory: 1Gi
  limits:
    cpu: 1000m
    memory: 2Gi
```

### 2. Using :latest Image Tag

**Impact:** Non-deterministic builds, cache issues

**Solution:**
```yaml
image: maven:3.8.6-openjdk-17  # Specific version
```

### 3. Monolithic Tasks

**Impact:** Poor reusability, difficult debugging

**Solution:** Break into focused, single-responsibility tasks

### 4. Ignoring Pipeline Pruner

**Impact:** etcd bloat, performance degradation

**Solution:** Configure automatic pruning (see above)

### 5. Sharing ReadWriteOnce PVC in Parallel

**Impact:** Pod scheduling failures

**Solution:**
- Use ReadWriteMany
- Use volumeClaimTemplate
- Avoid parallel tasks with shared workspace

### 6. Results Over 4KB

**Impact:** Task failures

**Solution:** Use workspaces for large data

### 7. Not Using Tekton Hub

**Impact:** Reinventing the wheel, maintenance burden

**Solution:** Check Hub first, contribute improvements

### 8. Overly Permissive RBAC

**Impact:** Security risks

**Solution:** Least privilege principle

### 9. Secrets in Logs

**Impact:** Credential exposure

**Solution:** Never print sensitive data

### 10. Not Testing Tasks Isolation

**Impact:** Pipeline failures

**Solution:** Test TaskRuns independently before using in pipelines

## Operating at Scale (Red Hat Lessons)

### 1. Configure Pipeline Pruner
Delete completed pods to free volumes and improve performance

### 2. Set Resource Requests/Limits
Scheduler performs poorly without them

### 3. Understand Workload Bottlenecks
Profile and optimize based on actual constraints

### 4. Avoid PVC Sharing Conflicts
Use volumeClaimTemplate or manage access modes carefully

### 5. Monitor etcd Performance
Prune old resources, use Tekton Results for long-term storage

### 6. Plan for Multi-AZ Clusters
PVCs are tied to specific availability zones

### 7. Use Tekton Resolvers
Replace ClusterTasks with cluster/hub/git resolvers

### 8. Implement Retry Strategies
Set retries for flaky network operations

### 9. Version Control Everything
Pin task/pipeline versions for stability

### 10. Test at Scale
Load test pipelines before production deployment

## Monitoring and Observability

### Tekton Dashboard

```bash
# Install Tekton Dashboard
kubectl apply -f https://storage.googleapis.com/tekton-releases/dashboard/latest/release.yaml

# Access dashboard
kubectl port-forward -n tekton-pipelines svc/tekton-dashboard 9097:9097
```

### Prometheus Metrics

**Tekton exports metrics:**
- `tekton_pipelinerun_duration_seconds`
- `tekton_pipelinerun_count`
- `tekton_taskrun_duration_seconds`
- `tekton_taskrun_count`

### Logging Best Practices

**Structured logging in tasks:**
```yaml
script: |
  echo "[INFO] Starting build"
  echo "[ERROR] Build failed: $ERROR_MSG"
  echo "[SUCCESS] Build completed"
```

**Aggregate logs:**
- Use Fluentd/Fluent Bit to collect logs
- Send to Elasticsearch, Loki, or CloudWatch
- Create dashboards in Grafana, Kibana

### Alerting

**Alert on:**
- Pipeline failure rate exceeding threshold
- Long-running pipelines
- Resource quota exhaustion
- PVC capacity
- Failed webhook deliveries

## Checklist

### Security Checklist
- [ ] Use workspace-based authentication
- [ ] Create dedicated ServiceAccounts
- [ ] Follow least privilege RBAC
- [ ] Run containers as non-root
- [ ] Scan images for vulnerabilities
- [ ] Rotate credentials regularly
- [ ] Don't commit secrets to Git
- [ ] Use signed commits

### Performance Checklist
- [ ] Set resource requests/limits on all steps
- [ ] Configure pipeline pruner
- [ ] Use caching (Kaniko, Maven, npm)
- [ ] Use volumeClaimTemplate for isolation
- [ ] Monitor resource usage
- [ ] Optimize Dockerfile layer caching
- [ ] Use specific image tags

### Reliability Checklist
- [ ] Set retries for flaky operations
- [ ] Use onError for optional tasks
- [ ] Implement timeouts
- [ ] Use finally tasks for cleanup
- [ ] Write idempotent tasks
- [ ] Test tasks in isolation
- [ ] Monitor pipeline success rate

### Maintainability Checklist
- [ ] Check Tekton Hub before custom tasks
- [ ] Parameterize all tasks
- [ ] Document parameters and workspaces
- [ ] Use resolvers (not ClusterTasks)
- [ ] Version tasks and pipelines
- [ ] Use consistent naming conventions
- [ ] Organize tasks by function
