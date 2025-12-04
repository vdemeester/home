# Context Workflow

Managing kubectl contexts, namespaces, and cluster switching.

## When to Use

- Switching between multiple Kubernetes clusters
- Managing different namespaces
- Setting default context or namespace
- Organizing multi-cluster workflows

## Context Management

### View Contexts

```bash
# List all contexts
kubectl config get-contexts

# Show current context
kubectl config current-context

# View full kubeconfig
kubectl config view

# View kubeconfig without revealing secrets
kubectl config view --minify
```

### Switch Contexts

```bash
# Switch to different context
kubectl config use-context <context-name>

# Example: Switch to production cluster
kubectl config use-context prod-cluster

# Example: Switch to development cluster
kubectl config use-context dev-cluster
```

### Create Context

```bash
# Create new context
kubectl config set-context <context-name> \
  --cluster=<cluster-name> \
  --user=<user-name> \
  --namespace=<namespace>

# Example: Create staging context
kubectl config set-context staging \
  --cluster=staging-cluster \
  --user=staging-admin \
  --namespace=staging
```

### Modify Context

```bash
# Set default namespace for current context
kubectl config set-context --current --namespace=<namespace>

# Example: Set namespace to production
kubectl config set-context --current --namespace=production

# Rename context
kubectl config rename-context <old-name> <new-name>
```

### Delete Context

```bash
# Delete context
kubectl config delete-context <context-name>

# Delete cluster
kubectl config delete-cluster <cluster-name>

# Delete user
kubectl config delete-user <user-name>
```

## Namespace Management

### View Namespaces

```bash
# List all namespaces
kubectl get namespaces
kubectl get ns

# Describe namespace
kubectl describe namespace <namespace>

# Get namespace with labels
kubectl get namespace --show-labels
```

### Create Namespace

```bash
# Create namespace imperatively
kubectl create namespace <namespace>

# Create namespace declaratively
cat <<EOF | kubectl apply -f -
apiVersion: v1
kind: Namespace
metadata:
  name: myapp
  labels:
    environment: production
    team: backend
EOF
```

### Set Default Namespace

```bash
# Set namespace for current context
kubectl config set-context --current --namespace=<namespace>

# Verify namespace
kubectl config view --minify | grep namespace

# Or check current context details
kubectl config get-contexts $(kubectl config current-context)
```

### Delete Namespace

```bash
# Delete namespace (WARNING: deletes all resources in namespace)
kubectl delete namespace <namespace>

# Delete with confirmation
kubectl delete namespace <namespace> --wait=true
```

## Multi-Cluster Workflows

### Context Naming Convention

**Recommended format:** `<environment>-<region>-<cluster>`

Examples:
- `prod-us-east-1`
- `staging-eu-west-1`
- `dev-local`

```bash
# Rename contexts to follow convention
kubectl config rename-context arn:aws:eks:us-east-1:123456789:cluster/prod prod-us-east-1
kubectl config rename-context docker-desktop dev-local
```

### Quick Context Switching

```bash
# Create aliases for common contexts
alias kctx='kubectl config use-context'
alias kns='kubectl config set-context --current --namespace'

# Usage
kctx prod-us-east-1
kns production
```

### Context Switcher Tools

**kubectx/kubens (recommended):**
```bash
# Install kubectx and kubens
# On NixOS, add to packages: pkgs.kubectx

# Switch context interactively
kubectx

# Switch to specific context
kubectx prod-us-east-1

# Switch to previous context
kubectx -

# Switch namespace interactively
kubens

# Switch to specific namespace
kubens production

# Switch to previous namespace
kubens -
```

### Verify Current Context

```bash
# Show current context and namespace
kubectl config current-context
kubectl config view --minify | grep namespace

# Or use kubectx/kubens
kubectx
kubens

# Show cluster info
kubectl cluster-info

# Verify you're in the right cluster
kubectl get nodes
```

## Kubeconfig Management

### Kubeconfig Location

**Default:** `~/.kube/config`

**Custom location:**
```bash
# Use custom kubeconfig
export KUBECONFIG=/path/to/custom/kubeconfig

# Use multiple kubeconfigs (merged)
export KUBECONFIG=~/.kube/config:~/.kube/config-cluster2

# Temporary kubeconfig for single command
kubectl --kubeconfig=/path/to/config get pods
```

### Merge Kubeconfigs

```bash
# Backup current config
cp ~/.kube/config ~/.kube/config.backup

# Merge multiple configs
KUBECONFIG=~/.kube/config:~/.kube/config-new kubectl config view --flatten > ~/.kube/config-merged
mv ~/.kube/config-merged ~/.kube/config
```

### Extract Context to Separate File

```bash
# Extract specific context
kubectl config view --minify --flatten --context=prod-cluster > prod-kubeconfig.yaml
```

### Secure Kubeconfig

```bash
# Set proper permissions
chmod 600 ~/.kube/config

# Verify permissions
ls -la ~/.kube/config
```

## Cluster Access Patterns

### EKS (AWS)

```bash
# Configure kubectl for EKS cluster
aws eks update-kubeconfig --region us-east-1 --name my-cluster

# With specific profile
aws eks update-kubeconfig --region us-east-1 --name my-cluster --profile prod

# With specific role
aws eks update-kubeconfig --region us-east-1 --name my-cluster --role-arn arn:aws:iam::123456789:role/EKSAdmin
```

### GKE (Google Cloud)

```bash
# Get GKE cluster credentials
gcloud container clusters get-credentials my-cluster --region us-central1

# With specific project
gcloud container clusters get-credentials my-cluster --region us-central1 --project my-project
```

### AKS (Azure)

```bash
# Get AKS cluster credentials
az aks get-credentials --resource-group myResourceGroup --name myAKSCluster

# Overwrite existing context
az aks get-credentials --resource-group myResourceGroup --name myAKSCluster --overwrite-existing
```

### K3s/Local Clusters

```bash
# K3s kubeconfig location
export KUBECONFIG=/etc/rancher/k3s/k3s.yaml

# Kind (Kubernetes in Docker)
kind export kubeconfig --name my-cluster

# Minikube
minikube kubectl -- get pods
# or
kubectl --context=minikube get pods
```

## Namespace Best Practices

### Standard Namespaces

- `default`: Avoid using for applications
- `kube-system`: System components only
- `kube-public`: Public resources
- `kube-node-lease`: Node heartbeats

### Recommended Namespace Structure

**By Environment:**
```
development
staging
production
```

**By Team:**
```
team-frontend
team-backend
team-data
```

**By Application:**
```
app-web
app-api
app-workers
```

**Hybrid Approach:**
```
prod-frontend
prod-backend
staging-frontend
staging-backend
dev-shared
```

### Namespace Isolation

```yaml
# Resource quota
apiVersion: v1
kind: ResourceQuota
metadata:
  name: compute-quota
  namespace: production
spec:
  hard:
    requests.cpu: "10"
    requests.memory: 20Gi
    limits.cpu: "20"
    limits.memory: 40Gi
    persistentvolumeclaims: "10"
---
# Limit range
apiVersion: v1
kind: LimitRange
metadata:
  name: cpu-memory-limits
  namespace: production
spec:
  limits:
  - max:
      cpu: "2"
      memory: 4Gi
    min:
      cpu: 100m
      memory: 128Mi
    default:
      cpu: 500m
      memory: 512Mi
    defaultRequest:
      cpu: 100m
      memory: 128Mi
    type: Container
---
# Network policy (deny all by default)
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: default-deny-all
  namespace: production
spec:
  podSelector: {}
  policyTypes:
  - Ingress
  - Egress
```

## Context Switching Safety

### Pre-flight Checks

```bash
# Always verify context before running commands
kubectl config current-context

# Verify you're in the right cluster
kubectl get nodes

# Check namespace
kubectl config view --minify | grep namespace
```

### Prevent Accidental Production Changes

```bash
# Create read-only context for production
kubectl config set-context prod-readonly \
  --cluster=prod-cluster \
  --user=readonly-user \
  --namespace=production

# Use separate terminal profiles for different environments
# Set terminal title based on context
export PS1='[\u@\h $(kubectl config current-context)] \W\$ '
```

### Shell Prompt Integration

```bash
# Add to ~/.bashrc or ~/.zshrc

# Show current k8s context in prompt
kube_ps1() {
  local ctx=$(kubectl config current-context 2>/dev/null)
  local ns=$(kubectl config view --minify --output 'jsonpath={..namespace}' 2>/dev/null)
  if [ -n "$ctx" ]; then
    echo "($ctx:${ns:-default})"
  fi
}

# Update PS1
export PS1='$(kube_ps1) \$ '

# Or use kube-ps1 tool
# On NixOS: pkgs.kube-ps1
source /path/to/kube-ps1.sh
PROMPT='$(kube_ps1)'$PROMPT
```

## Advanced Context Management

### Context Aliases

```bash
# Create short aliases
kubectl config set-context dev --cluster=dev-cluster --user=dev-user --namespace=development
kubectl config set-context prod --cluster=prod-cluster --user=prod-user --namespace=production

# Quick switching
kubectl config use-context dev
kubectl config use-context prod
```

### Per-Shell Context

```bash
# Override kubeconfig for specific shell session
export KUBECONFIG=~/.kube/config-dev

# Verify isolation
kubectl config current-context
```

### Context Validation Script

```bash
#!/bin/bash
# validate-context.sh

CONTEXT=$(kubectl config current-context)
NAMESPACE=$(kubectl config view --minify -o jsonpath='{..namespace}')

echo "Current context: $CONTEXT"
echo "Current namespace: ${NAMESPACE:-default}"
echo ""
echo "Cluster nodes:"
kubectl get nodes --no-headers | wc -l
echo ""
read -p "Is this the correct cluster? (yes/no): " confirm

if [ "$confirm" != "yes" ]; then
  echo "Aborting operation"
  exit 1
fi
```

## Troubleshooting Context Issues

### Context Not Found

```bash
# List available contexts
kubectl config get-contexts

# Verify kubeconfig file
kubectl config view

# Check KUBECONFIG environment variable
echo $KUBECONFIG
```

### Authentication Errors

```bash
# Refresh cloud provider credentials
# AWS
aws eks update-kubeconfig --region us-east-1 --name my-cluster

# GCP
gcloud container clusters get-credentials my-cluster --region us-central1

# Azure
az aks get-credentials --resource-group myRG --name myCluster
```

### Permission Errors

```bash
# Check current user
kubectl auth whoami

# Check permissions
kubectl auth can-i get pods
kubectl auth can-i create deployments
kubectl auth can-i '*' '*' --all-namespaces
```

## Best Practices Checklist

- [ ] Use descriptive context names
- [ ] Set default namespace for each context
- [ ] Verify context before running commands
- [ ] Use kubectx/kubens for easier switching
- [ ] Secure kubeconfig with proper permissions
- [ ] Create separate contexts for read-only access
- [ ] Use namespace isolation (quotas, limits, network policies)
- [ ] Add context to shell prompt
- [ ] Backup kubeconfig before modifications
- [ ] Document context naming conventions for team
