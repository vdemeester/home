# Debug Workflow

Debugging Tekton pipelines, tasks, and runs by leveraging Kubernetes debugging techniques.

## When to Use

- PipelineRun or TaskRun failing
- Tasks not starting or getting stuck
- Investigating pod failures in Tekton
- Debugging workspace issues
- Troubleshooting trigger problems

## Overview

**Tekton runs on Kubernetes**, so debugging Tekton involves:
1. **Tekton-specific debugging**: Using tkn CLI and Tekton resources
2. **Kubernetes debugging**: Using kubectl to inspect underlying pods (see **Kubernetes/Debug** workflow)

This workflow combines both approaches for comprehensive troubleshooting.

## Quick Diagnosis

### Check PipelineRun/TaskRun Status

```bash
# Get pipelinerun status
tkn pipelinerun describe <pipelinerun-name>
tkn pipelinerun describe --last

# Get taskrun status
tkn taskrun describe <taskrun-name>
tkn taskrun describe --last

# List recent runs
tkn pipelinerun list
tkn taskrun list
```

### View Logs

```bash
# Stream pipelinerun logs
tkn pipelinerun logs <pipelinerun-name> -f
tkn pipelinerun logs --last -f

# Stream taskrun logs
tkn taskrun logs <taskrun-name> -f
tkn taskrun logs --last -f

# View specific task logs in pipeline
tkn pipelinerun logs <pipelinerun-name> -t <task-name>
```

## Debugging Workflow

### 1. Identify the Problem

**Check PipelineRun status:**
```bash
# Quick status check
kubectl get pipelinerun <name>

# Detailed status
tkn pipelinerun describe <name>

# Check conditions
kubectl get pipelinerun <name> -o jsonpath='{.status.conditions[*]}'
```

**Common statuses:**
- `Running`: Pipeline is executing
- `Succeeded`: Pipeline completed successfully
- `Failed`: Pipeline failed
- `PipelineRunCancelled`: Pipeline was cancelled
- `PipelineRunTimeout`: Pipeline exceeded timeout

### 2. Find the Failing Task

```bash
# List all tasks in pipeline
tkn pipelinerun describe <name>

# Check which tasks failed
kubectl get pipelinerun <name> -o jsonpath='{.status.taskRuns}' | jq

# View task status
tkn taskrun describe <taskrun-name>
```

### 3. Examine Task Logs

```bash
# View all task logs
tkn pipelinerun logs <name> --all

# View specific failing task
tkn pipelinerun logs <name> -t <failing-task-name>

# View taskrun logs directly
tkn taskrun logs <taskrun-name>

# Get previous logs if pod crashed
kubectl logs <pod-name> -c step-<step-name> --previous
```

### 4. Inspect the Pod (Kubernetes Debug)

**Find the pod:**
```bash
# Find pods for pipelinerun
kubectl get pods -l tekton.dev/pipelineRun=<pipelinerun-name>

# Find pods for taskrun
kubectl get pods -l tekton.dev/taskRun=<taskrun-name>
```

**Use Kubernetes Debug workflow:**
```bash
# Check pod status (see Kubernetes/Debug)
kubectl get pods
kubectl describe pod <pod-name>

# Check pod events
kubectl get events --field-selector involvedObject.name=<pod-name>

# Check resource usage
kubectl top pod <pod-name>
```

### 5. Check Task Definition

```bash
# View task definition
kubectl get task <task-name> -o yaml

# Check parameters
tkn task describe <task-name>

# Verify workspace requirements
kubectl get task <task-name> -o jsonpath='{.spec.workspaces}'
```

### 6. Verify Workspaces

```bash
# Check workspace bindings in pipelinerun
kubectl get pipelinerun <name> -o jsonpath='{.spec.workspaces}'

# Check PVC status
kubectl get pvc

# Verify PVC is bound
kubectl get pvc <pvc-name> -o jsonpath='{.status.phase}'

# Check workspace contents (if pod still exists)
kubectl exec <pod-name> -c step-<step-name> -- ls -la /workspace
```

### 7. Check Results and Parameters

```bash
# View task results
tkn taskrun describe <taskrun-name> | grep -A 10 Results

# Get specific result
kubectl get taskrun <name> -o jsonpath='{.status.taskResults[?(@.name=="<result-name>")].value}'

# Check parameters passed
kubectl get pipelinerun <name> -o jsonpath='{.spec.params}'
```

## Common Tekton Issues

### Task Not Starting (Pending)

**Symptoms:**
- TaskRun shows `Pending` status
- Pod not created or stuck in `Pending`

**Debug:**
```bash
# Check taskrun status
tkn taskrun describe <taskrun-name>

# Check pod scheduling
kubectl describe pod <pod-name>

# Check events
kubectl get events --sort-by=.metadata.creationTimestamp
```

**Common causes:**
- Insufficient cluster resources (see **Kubernetes/Debug** - Pending Pods)
- PVC not bound
- Missing ServiceAccount
- Invalid workspace configuration
- Resource quota exceeded

**Solutions:**
```bash
# Check node resources
kubectl top nodes

# Check PVC
kubectl get pvc
kubectl describe pvc <pvc-name>

# Check ServiceAccount
kubectl get serviceaccount <sa-name>

# Check resource quotas
kubectl get resourcequota
```

### Task Failing with ImagePullBackOff

**Symptoms:**
- Pod containers can't pull images
- Events show `Failed to pull image`

**Debug:**
```bash
# Check pod events (see Kubernetes/Debug - ImagePullBackOff)
kubectl describe pod <pod-name> | grep -A 5 "Failed"

# Check task definition
kubectl get task <task-name> -o jsonpath='{.spec.steps[*].image}'
```

**Common causes:**
- Wrong image name or tag
- Private registry authentication missing
- Network issues

**Solutions:**
```bash
# Verify image exists
docker pull <image>

# Check imagePullSecrets in ServiceAccount
kubectl get serviceaccount <sa-name> -o yaml

# Verify docker config secret
kubectl get secret <docker-config-secret> -o jsonpath='{.data.\.dockerconfigjson}' | base64 -d
```

### Task Failing with CrashLoopBackOff

**Symptoms:**
- Step containers keep restarting
- Pod shows `CrashLoopBackOff`

**Debug:**
```bash
# Check previous logs (see Kubernetes/Debug - CrashLoopBackOff)
kubectl logs <pod-name> -c step-<step-name> --previous

# Check pod describe
kubectl describe pod <pod-name>

# Check task script
kubectl get task <task-name> -o jsonpath='{.spec.steps[*].script}'
```

**Common causes:**
- Script errors or missing dependencies
- Incorrect command/args
- Missing environment variables
- Resource limits too low
- Workspace permissions

**Solutions:**
```bash
# Check resource limits
kubectl get pod <pod-name> -o jsonpath='{.spec.containers[*].resources}'

# Check environment variables
kubectl get pod <pod-name> -o jsonpath='{.spec.containers[*].env}'

# Exec into workspace to check (if pod running)
kubectl exec <pod-name> -c step-<step-name> -- ls -la /workspace
```

### Workspace Issues

**Symptoms:**
- Tasks can't access workspaces
- Permission denied errors
- Data not shared between tasks

**Debug:**
```bash
# Check workspace bindings
kubectl get pipelinerun <name> -o jsonpath='{.spec.workspaces}'
kubectl get taskrun <name> -o jsonpath='{.spec.workspaces}'

# Check PVC access mode
kubectl get pvc <pvc-name> -o jsonpath='{.spec.accessModes}'

# Check if PVC is bound
kubectl get pvc <pvc-name>

# Check volumes in pod
kubectl get pod <pod-name> -o jsonpath='{.spec.volumes}'

# Check workspace mounts
kubectl get pod <pod-name> -o jsonpath='{.spec.containers[*].volumeMounts}'
```

**Common causes:**
- PVC not created or not bound
- ReadWriteOnce PVC shared across nodes (parallel tasks)
- Incorrect workspace name mapping
- PVC size too small
- Permission issues

**Solutions:**
```bash
# Create missing PVC
kubectl apply -f pvc.yaml

# Use volumeClaimTemplate for isolation
# (see Tekton/Pipelines workflow)

# Check PVC capacity
kubectl get pvc <pvc-name> -o jsonpath='{.status.capacity.storage}'

# Use ReadWriteMany if available
# Or ensure tasks run sequentially
```

### Pipeline Timeout

**Symptoms:**
- PipelineRun shows `PipelineRunTimeout`
- Pipeline stopped before completion

**Debug:**
```bash
# Check timeout configuration
kubectl get pipeline <name> -o jsonpath='{.spec.timeouts}'

# Check pipelinerun duration
kubectl get pipelinerun <name> -o jsonpath='{.status.startTime}'
kubectl get pipelinerun <name> -o jsonpath='{.status.completionTime}'

# Check which task was running
tkn pipelinerun describe <name>
```

**Solutions:**
```bash
# Increase timeout in pipeline
spec:
  timeouts:
    pipeline: 2h
    tasks: 1h

# Or in pipelinerun
spec:
  timeout: 2h
```

### Results Not Available

**Symptoms:**
- Pipeline can't access task results
- Result referenced but empty

**Debug:**
```bash
# Check if result was emitted
tkn taskrun describe <taskrun-name> | grep -A 10 Results

# Get result value
kubectl get taskrun <name> -o jsonpath='{.status.taskResults}'

# Check result size (must be <4KB)
kubectl logs <pod-name> -c step-<step-name>
```

**Common causes:**
- Task didn't write to result path
- Result exceeds 4KB limit
- Step failed before writing result

**Solutions:**
```bash
# Verify result is written in task
script: |
  echo -n "value" > $(results.result-name.path)

# For large data, use workspace instead
# (see Tekton/Tasks workflow)
```

### Trigger Not Firing

**Symptoms:**
- Webhook received but no PipelineRun created
- EventListener not responding

**Debug:**
```bash
# Check EventListener pod
kubectl get pods -l eventlistener=<listener-name>

# Check EventListener logs
kubectl logs -l eventlistener=<listener-name>

# Check EventListener service
kubectl get svc el-<listener-name>

# Test webhook endpoint
kubectl port-forward svc/el-<listener-name> 8080:8080
curl -X POST http://localhost:8080 -d '{}'
```

**Common causes:**
- EventListener pod not running
- Webhook secret mismatch
- TriggerBinding extraction error
- RBAC permissions missing
- Interceptor filtering event

**Solutions:**
```bash
# Check EventListener status
kubectl describe eventlistener <listener-name>

# Check RBAC
kubectl auth can-i create pipelineruns --as=system:serviceaccount:<namespace>:<sa-name>

# Check webhook secret
kubectl get secret <webhook-secret> -o yaml

# Review interceptor filters
kubectl get eventlistener <listener-name> -o jsonpath='{.spec.triggers[*].interceptors}'
```

## Advanced Debugging

### Debug Containers (Kubernetes 1.23+)

```bash
# Add debug container to running pod
kubectl debug <pod-name> -it --image=busybox --target=step-<step-name>

# Create debug copy of pod
kubectl debug <pod-name> -it --copy-to=<debug-pod-name> --container=debug
```

### Check Tekton Controller Logs

```bash
# Get Tekton controller logs
kubectl logs -n tekton-pipelines -l app=tekton-pipelines-controller

# Get webhook logs
kubectl logs -n tekton-pipelines -l app=tekton-pipelines-webhook

# Get triggers controller logs
kubectl logs -n tekton-pipelines -l app.kubernetes.io/component=controller
```

### Inspect etcd for Resource Issues

```bash
# Check if resources are being pruned
kubectl get pipelineruns --all-namespaces | wc -l

# Configure pruner if needed
# (see Tekton/BestPractices workflow)
```

### Enable Debug Logging

```bash
# Set Tekton controller to debug level
kubectl edit configmap config-logging -n tekton-pipelines

# Add:
# zap-logger-config: |
#   level: debug
```

## Debugging Checklist

- [ ] Check PipelineRun/TaskRun status with tkn CLI
- [ ] View logs with `tkn pipelinerun logs` or `tkn taskrun logs`
- [ ] Identify failing task in pipeline
- [ ] Inspect pod status with kubectl (see **Kubernetes/Debug**)
- [ ] Check pod events and describe output
- [ ] Verify task definition and parameters
- [ ] Check workspace configuration and PVC status
- [ ] Validate ServiceAccount and RBAC permissions
- [ ] Review resource requests and limits
- [ ] Check for timeout issues
- [ ] Verify results are within 4KB limit
- [ ] For triggers: check EventListener logs

## Integration with Kubernetes Debug

**When debugging Tekton, always remember:**
- Tekton tasks run as Kubernetes pods
- Use **Kubernetes/Debug** workflow for pod-level issues
- Common Kubernetes issues apply: ImagePullBackOff, CrashLoopBackOff, OOMKilled, Pending
- Kubernetes debugging tools work: `kubectl describe`, `kubectl logs`, `kubectl exec`, `kubectl debug`

**Debugging flow:**
1. Start with Tekton-specific tools (tkn CLI)
2. Drill down to Kubernetes pod debugging (kubectl)
3. Check Tekton controller logs for system issues
4. Review Tekton configuration (tasks, pipelines, triggers)

## Useful Commands Reference

### Quick Status

```bash
# Last pipelinerun
tkn pipelinerun describe --last

# Failed pipelineruns
tkn pipelinerun list | grep Failed

# Running pipelineruns
kubectl get pipelineruns -o wide | grep Running
```

### Log Streaming

```bash
# Follow last pipelinerun
tkn pipelinerun logs --last -f

# All tasks in pipeline
tkn pipelinerun logs <name> --all

# With timestamps
tkn pipelinerun logs <name> -f --timestamps
```

### Pod Inspection

```bash
# Find Tekton pods
kubectl get pods -l tekton.dev/pipelineRun

# Describe pod (see Kubernetes/Debug)
kubectl describe pod <pod-name>

# Pod resource usage
kubectl top pod <pod-name>
```

### Cleanup Failed Runs

```bash
# Delete failed pipelineruns
tkn pipelinerun delete --all --failed -f

# Keep last N runs
tkn pipelinerun delete --keep 5 -f
```

## Monitoring

**Metrics to track:**
- Pipeline success rate
- Pipeline duration
- Task failure patterns
- Resource usage (CPU, memory)
- PVC capacity

**Alerting on:**
- Repeated pipeline failures
- Timeout issues
- Resource exhaustion
- EventListener downtime

## Example Debug Session

```bash
# 1. Check what's failing
tkn pipelinerun list | head -n 5

# 2. Describe the failed run
tkn pipelinerun describe my-pipeline-run-abc123

# 3. Identify failing task
# Output shows: build-image task failed

# 4. Check task logs
tkn pipelinerun logs my-pipeline-run-abc123 -t build-image

# 5. Find the pod
kubectl get pods -l tekton.dev/taskRun=my-pipeline-run-abc123-build-image

# 6. Use Kubernetes debugging (see Kubernetes/Debug)
kubectl describe pod my-pipeline-run-abc123-build-image-pod-abc123
kubectl logs my-pipeline-run-abc123-build-image-pod-abc123 -c step-build --previous

# 7. Check workspace if needed
kubectl exec my-pipeline-run-abc123-build-image-pod-abc123 -c step-build -- ls -la /workspace/source

# 8. Review task definition
kubectl get task build-image -o yaml

# 9. Check related resources
kubectl get pvc
kubectl get serviceaccount
```

## See Also

- **Kubernetes/Debug**: Pod-level debugging techniques
- **Tekton/TknCli**: tkn CLI command reference
- **Tekton/Tasks**: Task definition troubleshooting
- **Tekton/Pipelines**: Pipeline execution debugging
- **Tekton/BestPractices**: Preventing common issues
