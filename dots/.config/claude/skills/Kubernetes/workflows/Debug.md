# Debug Workflow

Systematic debugging workflow for Kubernetes pods and resources.

## When to Use

- Pod is failing, crashing, or in error state
- Application not behaving as expected in cluster
- Need to troubleshoot connectivity or performance issues
- Investigating resource problems

## Debugging Steps

### 1. Check Pod Status

```bash
# Get all pods in current namespace
kubectl get pods

# Get pods in specific namespace
kubectl get pods -n <namespace>

# Get pods with more details
kubectl get pods -o wide

# Watch pod status changes
kubectl get pods -w
```

**Common Pod States:**
- `Pending`: Waiting to be scheduled
- `ContainerCreating`: Container image being pulled/created
- `Running`: Pod is running
- `CrashLoopBackOff`: Container keeps crashing
- `Error`: Container exited with error
- `ImagePullBackOff`: Cannot pull container image
- `Evicted`: Pod evicted due to resource pressure

### 2. Describe Pod for Details

```bash
# Get detailed pod information
kubectl describe pod <pod-name>

# Focus on specific sections
kubectl describe pod <pod-name> | grep -A 10 Events
kubectl describe pod <pod-name> | grep -A 5 State
```

**Look for:**
- Events (errors, warnings, scheduling issues)
- Container states and restart counts
- Resource requests/limits vs actual usage
- Node assignment and conditions
- Volume mount issues

### 3. Check Logs

```bash
# View current logs
kubectl logs <pod-name>

# View logs from specific container in multi-container pod
kubectl logs <pod-name> -c <container-name>

# Follow logs in real-time
kubectl logs <pod-name> -f

# View previous container logs (after crash)
kubectl logs <pod-name> --previous

# Get last N lines
kubectl logs <pod-name> --tail=100

# Show timestamps
kubectl logs <pod-name> --timestamps
```

### 4. Check Events

```bash
# Get recent events sorted by time
kubectl get events --sort-by=.metadata.creationTimestamp

# Watch events in real-time
kubectl get events -w

# Get events for specific namespace
kubectl get events -n <namespace>

# Filter events by type
kubectl get events --field-selector type=Warning
```

### 5. Exec into Pod

```bash
# Get shell in pod
kubectl exec -it <pod-name> -- /bin/sh
kubectl exec -it <pod-name> -- /bin/bash

# Run specific command
kubectl exec <pod-name> -- ls /app
kubectl exec <pod-name> -- env
kubectl exec <pod-name> -- cat /etc/resolv.conf

# Exec into specific container
kubectl exec -it <pod-name> -c <container-name> -- /bin/sh
```

**Inside pod, check:**
- Environment variables: `env`
- DNS resolution: `nslookup service-name`
- Network connectivity: `wget -O- http://service:port`
- File permissions: `ls -la /app`
- Disk usage: `df -h`
- Process list: `ps aux`

### 6. Port Forward for Testing

```bash
# Forward local port to pod port
kubectl port-forward pod/<pod-name> 8080:8080

# Forward to service
kubectl port-forward service/<service-name> 8080:80

# Forward in background
kubectl port-forward pod/<pod-name> 8080:8080 &

# Test with curl
curl http://localhost:8080
```

### 7. Check Resource Usage

```bash
# Get pod resource usage (requires metrics-server)
kubectl top pod <pod-name>

# Get all pods resource usage
kubectl top pods

# Get node resource usage
kubectl top nodes
```

### 8. Check Related Resources

```bash
# Check deployment
kubectl get deployment <deployment-name>
kubectl describe deployment <deployment-name>

# Check replica set
kubectl get rs
kubectl describe rs <replicaset-name>

# Check service
kubectl get svc
kubectl describe svc <service-name>

# Check endpoints
kubectl get endpoints <service-name>

# Check configmaps and secrets
kubectl get configmap
kubectl get secret
kubectl describe configmap <name>
```

### 9. Network Debugging

```bash
# Check service endpoints
kubectl get endpoints

# Test DNS resolution from within pod
kubectl exec <pod-name> -- nslookup kubernetes.default
kubectl exec <pod-name> -- nslookup <service-name>

# Check network policies
kubectl get networkpolicies
kubectl describe networkpolicy <policy-name>

# Test connectivity between pods
kubectl exec <pod-name> -- wget -O- http://<service-name>:<port>
```

### 10. Check RBAC Permissions

```bash
# Check if service account can perform action
kubectl auth can-i get pods --as=system:serviceaccount:<namespace>:<sa-name>

# Get service account details
kubectl get serviceaccount <sa-name> -o yaml

# Check role bindings
kubectl get rolebindings
kubectl describe rolebinding <binding-name>
```

## Common Issues and Solutions

### ImagePullBackOff

**Causes:**
- Image doesn't exist
- Image tag is wrong
- Private registry authentication failed
- Network issues pulling image

**Debug:**
```bash
kubectl describe pod <pod-name> | grep -A 5 "Failed"
kubectl get events | grep "Failed to pull"
```

**Solutions:**
- Verify image name and tag
- Check imagePullSecrets
- Test image pull manually: `docker pull <image>`

### CrashLoopBackOff

**Causes:**
- Application exits immediately
- Missing dependencies or configuration
- Resource limits too low
- Failed health checks

**Debug:**
```bash
kubectl logs <pod-name> --previous
kubectl describe pod <pod-name>
```

**Solutions:**
- Check application logs for errors
- Verify environment variables and config
- Increase resource limits
- Check liveness probe configuration

### Pending Pods

**Causes:**
- Insufficient node resources
- Node selector/affinity not matching
- PersistentVolumeClaim not bound
- Pod security policy blocking

**Debug:**
```bash
kubectl describe pod <pod-name> | grep -A 10 Events
kubectl get nodes
kubectl top nodes
```

**Solutions:**
- Scale cluster or reduce resource requests
- Check node labels and selectors
- Verify PVC status
- Review pod security policies

### OOMKilled (Out of Memory)

**Causes:**
- Memory limit too low
- Memory leak in application
- Unexpected memory usage spike

**Debug:**
```bash
kubectl describe pod <pod-name> | grep -i "OOMKilled"
kubectl top pod <pod-name>
```

**Solutions:**
- Increase memory limits
- Investigate application memory usage
- Add memory profiling to application

## Debugging Checklist

- [ ] Check pod status and events
- [ ] Review pod description for errors
- [ ] Examine current and previous logs
- [ ] Verify resource requests and limits
- [ ] Test network connectivity
- [ ] Validate environment variables and config
- [ ] Check RBAC permissions
- [ ] Verify image and pull secrets
- [ ] Review health probe configuration
- [ ] Check node resources and scheduling

## Advanced Debugging

### Enable Debug Container (Kubernetes 1.23+)

```bash
# Add ephemeral debug container to running pod
kubectl debug <pod-name> -it --image=busybox --target=<container-name>

# Create debug copy of pod
kubectl debug <pod-name> -it --copy-to=<debug-pod-name> --container=debug
```

### Node Debugging

```bash
# SSH to node (if accessible)
kubectl get nodes -o wide

# Create privileged pod on specific node
kubectl debug node/<node-name> -it --image=ubuntu
```

### Check API Server Logs

```bash
# Get API server logs (if running as pod)
kubectl logs -n kube-system kube-apiserver-<node-name>

# Get controller manager logs
kubectl logs -n kube-system kube-controller-manager-<node-name>

# Get scheduler logs
kubectl logs -n kube-system kube-scheduler-<node-name>
```

## Monitoring Recommendations

**Install metrics-server:**
```bash
kubectl apply -f https://github.com/kubernetes-sigs/metrics-server/releases/latest/download/components.yaml
```

**Use kubectl plugins:**
- `kubectl-debug`: Enhanced debugging capabilities
- `kubectl-trace`: Trace syscalls and kernel events
- `kubectl-df`: Show disk usage in pods
- `kubectl-view-allocations`: View resource allocations

## Example Debugging Session

```bash
# 1. Find problematic pod
kubectl get pods | grep -v Running

# 2. Get details
kubectl describe pod myapp-5d4b6c8f9-xz2k4

# 3. Check logs
kubectl logs myapp-5d4b6c8f9-xz2k4 --previous

# 4. Check events
kubectl get events --field-selector involvedObject.name=myapp-5d4b6c8f9-xz2k4

# 5. Test connectivity
kubectl exec myapp-5d4b6c8f9-xz2k4 -- wget -O- http://backend-service:8080

# 6. Check resources
kubectl top pod myapp-5d4b6c8f9-xz2k4

# 7. If needed, exec into pod
kubectl exec -it myapp-5d4b6c8f9-xz2k4 -- /bin/sh
```
