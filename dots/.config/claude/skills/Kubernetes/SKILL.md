---
name: Kubernetes
description: Kubernetes development and operations best practices. USE WHEN working with kubectl commands, K8s manifests, deployments, debugging pods, managing resources, RBAC, or cluster operations.
---

# Kubernetes

Expert guidance on Kubernetes operations, kubectl command-line usage, and cloud-native development best practices.

## Workflow Routing

**When executing a workflow, output this notification directly:**

```
Running the **WorkflowName** workflow from the **Kubernetes** skill...
```

| Workflow | Trigger | File |
|----------|---------|------|
| **Debug** | "debug pod", "troubleshoot", "pod logs", "pod errors" | `workflows/Debug.md` |
| **Deploy** | "deploy", "apply manifests", "rollout", "update deployment" | `workflows/Deploy.md` |
| **Context** | "kubectl context", "namespace", "switch cluster" | `workflows/Context.md` |
| **Resources** | "create resource", "manifest", "yaml", "configmap", "secret" | `workflows/Resources.md` |
| **Security** | "RBAC", "pod security", "network policy", "security best practices" | `workflows/Security.md` |
| **Scaling** | "scale deployment", "HPA", "autoscaling", "replicas" | `workflows/Scaling.md` |

## kubectl Productivity

### Essential Aliases

```bash
alias k=kubectl
alias kgp='kubectl get pods'
alias kgs='kubectl get svc'
alias kgd='kubectl get deployments'
alias kgn='kubectl get nodes'
alias kdp='kubectl describe pod'
alias kdd='kubectl describe deployment'
alias kl='kubectl logs'
alias kx='kubectl exec -it'
alias kaf='kubectl apply -f'
alias kdel='kubectl delete'
```

### Quick Commands

**Get resources with custom output:**
```bash
kubectl get pods -o wide
kubectl get pods -o yaml
kubectl get pods -o json | jq '.items[].metadata.name'
kubectl get pods --sort-by=.metadata.creationTimestamp
```

**Watch resources:**
```bash
kubectl get pods -w
kubectl get events -w
```

**Context and namespace:**
```bash
kubectl config get-contexts
kubectl config use-context <context>
kubectl config set-context --current --namespace=<namespace>
```

## Key Best Practices

### Resource Definitions

**Always specify:**
- Resource limits and requests
- Readiness and liveness probes
- Labels and selectors
- Pod disruption budgets for production
- Security context (non-root user, read-only filesystem)

**Example deployment manifest:**
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: myapp
  labels:
    app: myapp
    version: v1.0.0
spec:
  replicas: 3
  selector:
    matchLabels:
      app: myapp
  template:
    metadata:
      labels:
        app: myapp
        version: v1.0.0
    spec:
      securityContext:
        runAsNonRoot: true
        runAsUser: 1000
        fsGroup: 1000
      containers:
      - name: myapp
        image: myapp:v1.0.0
        ports:
        - containerPort: 8080
        resources:
          requests:
            cpu: 100m
            memory: 128Mi
          limits:
            cpu: 500m
            memory: 512Mi
        livenessProbe:
          httpGet:
            path: /healthz
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 8080
          initialDelaySeconds: 5
          periodSeconds: 5
        securityContext:
          allowPrivilegeEscalation: false
          readOnlyRootFilesystem: true
        volumeMounts:
        - name: tmp
          mountPath: /tmp
      volumes:
      - name: tmp
        emptyDir: {}
```

### Recommended Labels

```yaml
metadata:
  labels:
    app.kubernetes.io/name: myapp
    app.kubernetes.io/instance: myapp-prod
    app.kubernetes.io/version: "1.0.0"
    app.kubernetes.io/component: backend
    app.kubernetes.io/part-of: myplatform
    app.kubernetes.io/managed-by: kubectl
```

### Anti-Patterns to Avoid

1. **NEVER use `:latest` tag** - Always use specific version tags
2. **NEVER skip health probes** - Always define readiness/liveness probes
3. **NEVER run as root** - Use non-root user in security context
4. **NEVER hardcode configuration** - Use ConfigMaps and Secrets
5. **NEVER ignore resource limits** - Always set requests and limits
6. **NEVER use default namespace** - Create and use specific namespaces
7. **NEVER skip labels** - Use consistent labeling strategy
8. **NEVER deploy without testing** - Use `kubectl apply --dry-run=client`
9. **NEVER expose secrets in env** - Use volume mounts for secrets
10. **NEVER skip RBAC** - Use principle of least privilege

## Security Checklist

- [ ] Run containers as non-root user
- [ ] Use read-only root filesystem
- [ ] Drop all capabilities, add only required ones
- [ ] Enable Pod Security Standards (Baseline or Restricted)
- [ ] Use Network Policies to restrict traffic
- [ ] Scan images for vulnerabilities
- [ ] Use secrets for sensitive data (never in env vars directly)
- [ ] Enable RBAC with least privilege
- [ ] Use service accounts per application
- [ ] Enable audit logging

## Debugging Workflow

1. **Check pod status:** `kubectl get pods`
2. **Describe pod:** `kubectl describe pod <pod-name>`
3. **View logs:** `kubectl logs <pod-name> [-c <container>]`
4. **Previous logs:** `kubectl logs <pod-name> --previous`
5. **Exec into pod:** `kubectl exec -it <pod-name> -- /bin/sh`
6. **Check events:** `kubectl get events --sort-by=.metadata.creationTimestamp`
7. **Port forward:** `kubectl port-forward pod/<pod-name> 8080:8080`

## Examples

**Example 1: Debug failing pod**
```
User: "My pod is in CrashLoopBackOff, help me debug it"
→ Invokes Debug workflow
→ Checks pod status and events
→ Reviews logs for errors
→ Validates resource definitions
→ Identifies root cause and suggests fix
```

**Example 2: Deploy application**
```
User: "Deploy my application to Kubernetes with best practices"
→ Invokes Deploy workflow
→ Creates manifest with security contexts
→ Adds health probes and resource limits
→ Validates with dry-run
→ Applies and monitors rollout
```

**Example 3: Set up RBAC**
```
User: "Create RBAC for my application with minimal permissions"
→ Invokes Security workflow
→ Creates ServiceAccount
→ Defines Role with specific permissions
→ Creates RoleBinding
→ Tests permissions
```

## Integration

This skill integrates with:
- **Git skill**: For version controlling manifests and GitOps workflows
- **Golang skill**: For developing Kubernetes operators and controllers
- **Nix skill**: For managing Kubernetes tools in NixOS environments
- **Homelab skill**: For managing K3s/K8s clusters in homelab setup
