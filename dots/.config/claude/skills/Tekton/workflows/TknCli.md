# TknCli Workflow

Tekton CLI (tkn) command reference and usage patterns for managing pipelines, tasks, and debugging.

## When to Use

- Running tkn commands
- Viewing pipeline/task logs
- Managing pipeline runs
- Debugging Tekton workflows
- Monitoring execution status

## Installation

### Package Managers

```bash
# macOS
brew install tektoncd-cli

# Windows
choco install tektoncd-cli

# Linux (deb-based)
curl -LO https://github.com/tektoncd/cli/releases/download/v0.35.0/tektoncd-cli-0.35.0_Linux-64bit.deb
sudo dpkg -i tektoncd-cli-0.35.0_Linux-64bit.deb

# Linux (rpm-based)
curl -LO https://github.com/tektoncd/cli/releases/download/v0.35.0/tektoncd-cli-0.35.0_Linux-64bit.rpm
sudo rpm -Uvh tektoncd-cli-0.35.0_Linux-64bit.rpm

# Linux (tarball)
curl -LO https://github.com/tektoncd/cli/releases/download/v0.35.0/tkn_0.35.0_Linux_x86_64.tar.gz
sudo tar xvzf tkn_0.35.0_Linux_x86_64.tar.gz -C /usr/local/bin/ tkn
```

### As kubectl Plugin

```bash
# Create symlink
ln -s /usr/local/bin/tkn /usr/local/bin/kubectl-tkn

# Use as kubectl plugin
kubectl tkn version
```

### Shell Completion

```bash
# Bash
source <(tkn completion bash)
echo "source <(tkn completion bash)" >> ~/.bashrc

# Zsh
source <(tkn completion zsh)
echo "source <(tkn completion zsh)" >> ~/.zshrc

# Fish
tkn completion fish | source
echo "tkn completion fish | source" >> ~/.config/fish/config.fish
```

### Verify Installation

```bash
tkn version

# Expected output:
# Client version: 0.35.0
# Pipeline version: v0.53.0
# Triggers version: v0.25.0
```

## Pipeline Commands

### List Pipelines

```bash
# List all pipelines
tkn pipeline list

# List with namespace
tkn pipeline list -n <namespace>

# List all namespaces
tkn pipeline list --all-namespaces
```

### Describe Pipeline

```bash
# Show pipeline details
tkn pipeline describe <pipeline-name>

# Show with full YAML
tkn pipeline describe <pipeline-name> -o yaml

# Show with JSON
tkn pipeline describe <pipeline-name> -o json
```

### Start Pipeline

```bash
# Start pipeline interactively
tkn pipeline start <pipeline-name>

# Start with parameters
tkn pipeline start <pipeline-name> \
  -p git-url=https://github.com/org/repo \
  -p image-name=myregistry.com/myimage:latest

# Start with workspace
tkn pipeline start <pipeline-name> \
  -w name=shared-data,claimName=my-pvc

# Start with ServiceAccount
tkn pipeline start <pipeline-name> \
  --serviceaccount=pipeline-sa

# Start and follow logs
tkn pipeline start <pipeline-name> --showlog

# Use last pipelinerun's parameters
tkn pipeline start <pipeline-name> --last

# Dry run (generate YAML without creating)
tkn pipeline start <pipeline-name> \
  -p git-url=https://github.com/org/repo \
  --dry-run > pipelinerun.yaml
```

### Delete Pipeline

```bash
# Delete pipeline
tkn pipeline delete <pipeline-name>

# Delete without confirmation
tkn pipeline delete <pipeline-name> -f

# Delete all pipelines
tkn pipeline delete --all

# Delete with associated runs
tkn pipeline delete <pipeline-name> --delete-prs
```

## PipelineRun Commands

### List PipelineRuns

```bash
# List all pipelineruns
tkn pipelinerun list

# List for specific pipeline
tkn pipelinerun list -p <pipeline-name>

# List with timestamps
tkn pipelinerun list -o wide

# Limit results
tkn pipelinerun list --limit 10
```

### Describe PipelineRun

```bash
# Describe last pipelinerun
tkn pipelinerun describe --last

# Describe specific pipelinerun
tkn pipelinerun describe <pipelinerun-name>

# Show full YAML
tkn pipelinerun describe <pipelinerun-name> -o yaml

# Show results
tkn pipelinerun describe <pipelinerun-name> | grep -A 10 Results
```

### View Logs

```bash
# View logs from last pipelinerun
tkn pipelinerun logs --last

# Follow logs from last run
tkn pipelinerun logs --last -f

# View specific pipelinerun logs
tkn pipelinerun logs <pipelinerun-name>

# Follow specific pipelinerun
tkn pipelinerun logs <pipelinerun-name> -f

# View logs for specific task
tkn pipelinerun logs <pipelinerun-name> -t <task-name>

# View all task logs
tkn pipelinerun logs <pipelinerun-name> --all

# Stream logs with timestamps
tkn pipelinerun logs --last -f --timestamps
```

### Cancel PipelineRun

```bash
# Cancel last pipelinerun
tkn pipelinerun cancel --last

# Cancel specific pipelinerun
tkn pipelinerun cancel <pipelinerun-name>

# Cancel without confirmation
tkn pipelinerun cancel <pipelinerun-name> -f
```

### Delete PipelineRun

```bash
# Delete specific pipelinerun
tkn pipelinerun delete <pipelinerun-name>

# Delete last pipelinerun
tkn pipelinerun delete --last

# Delete all failed pipelineruns
tkn pipelinerun delete --all -f --failed

# Delete all pipelineruns for pipeline
tkn pipelinerun delete --pipeline <pipeline-name>

# Keep last N runs
tkn pipelinerun delete --keep 5

# Delete runs older than N minutes
tkn pipelinerun delete --keep-since 60
```

## Task Commands

### List Tasks

```bash
# List all tasks
tkn task list

# List in namespace
tkn task list -n <namespace>

# List all namespaces
tkn task list --all-namespaces
```

### Describe Task

```bash
# Describe task
tkn task describe <task-name>

# Show YAML
tkn task describe <task-name> -o yaml

# Show parameters and workspaces
tkn task describe <task-name> | grep -E "(Parameters|Workspaces)" -A 10
```

### Start Task

```bash
# Start task interactively
tkn task start <task-name>

# Start with parameters
tkn task start <task-name> \
  -p greeting=Hello \
  -p name=World

# Start with workspace
tkn task start <task-name> \
  -w name=source,claimName=my-pvc

# Start and show logs
tkn task start <task-name> --showlog

# Use last taskrun's parameters
tkn task start <task-name> --last
```

### Delete Task

```bash
# Delete task
tkn task delete <task-name>

# Delete without confirmation
tkn task delete <task-name> -f

# Delete with associated taskruns
tkn task delete <task-name> --delete-trs
```

## TaskRun Commands

### List TaskRuns

```bash
# List all taskruns
tkn taskrun list

# List for specific task
tkn taskrun list -t <task-name>

# List with details
tkn taskrun list -o wide

# Limit results
tkn taskrun list --limit 10
```

### Describe TaskRun

```bash
# Describe last taskrun
tkn taskrun describe --last

# Describe specific taskrun
tkn taskrun describe <taskrun-name>

# Show YAML
tkn taskrun describe <taskrun-name> -o yaml
```

### View TaskRun Logs

```bash
# View last taskrun logs
tkn taskrun logs --last

# Follow last taskrun
tkn taskrun logs --last -f

# View specific taskrun
tkn taskrun logs <taskrun-name>

# Follow specific taskrun
tkn taskrun logs <taskrun-name> -f
```

### Delete TaskRun

```bash
# Delete taskrun
tkn taskrun delete <taskrun-name>

# Delete last taskrun
tkn taskrun delete --last

# Delete all taskruns
tkn taskrun delete --all
```

## ClusterTask Commands (Deprecated)

**IMPORTANT:** ClusterTasks are deprecated. Use resolvers instead.

```bash
# List clustertasks (still works but deprecated)
tkn clustertask list

# Describe clustertask
tkn clustertask describe <clustertask-name>

# Start clustertask
tkn clustertask start <clustertask-name>
```

**Migration to resolvers:**
```yaml
# Old (ClusterTask)
taskRef:
  name: git-clone
  kind: ClusterTask

# New (Cluster resolver)
taskRef:
  resolver: cluster
  params:
    - name: name
      value: git-clone
    - name: namespace
      value: tekton-tasks

# Or use Hub resolver
taskRef:
  resolver: hub
  params:
    - name: name
      value: git-clone
    - name: version
      value: "0.10.0"
```

## Hub Commands

### Search Hub

```bash
# Search for tasks
tkn hub search git

# Search with tags
tkn hub search --tags git

# Search with limit
tkn hub search --limit 10
```

### Get Hub Task Info

```bash
# Get task info
tkn hub info task git-clone

# Get specific version
tkn hub info task git-clone --version 0.10.0
```

### Install from Hub

```bash
# Install task from hub
tkn hub install task git-clone

# Install specific version
tkn hub install task git-clone --version 0.10.0

# Install to specific namespace
tkn hub install task git-clone -n tekton-tasks
```

## Debugging Workflows

### Check Pipeline Status

```bash
# Get overview
tkn pipelinerun describe --last

# Check task statuses
tkn pipelinerun describe --last | grep "TASKRUNS"

# View conditions
kubectl get pipelinerun <name> -o jsonpath='{.status.conditions}'
```

### View Detailed Logs

```bash
# All task logs with timestamps
tkn pipelinerun logs --last -f --all --timestamps

# Specific task logs
tkn pipelinerun logs --last -t <task-name> -f

# Specific step logs (use kubectl)
kubectl logs <pod-name> -c step-<step-name>
```

### Check Events

```bash
# View events for namespace
kubectl get events --sort-by=.metadata.creationTimestamp

# Filter for Tekton events
kubectl get events | grep -E "(Task|Pipeline)"

# Watch events
kubectl get events -w
```

### Inspect Pods

```bash
# Find taskrun pods
kubectl get pods | grep <pipelinerun-name>

# Describe pod
kubectl describe pod <pod-name>

# Check init containers
kubectl get pod <pod-name> -o jsonpath='{.spec.initContainers[*].name}'

# Check step containers
kubectl get pod <pod-name> -o jsonpath='{.spec.containers[*].name}'
```

### Debug Failed Tasks

```bash
# Describe failed taskrun
tkn taskrun describe <taskrun-name>

# View pod conditions
kubectl get pod <pod-name> -o jsonpath='{.status.conditions}' | jq

# Check step status
kubectl get pod <pod-name> -o jsonpath='{.status.containerStatuses}' | jq

# View terminated container info
kubectl get pod <pod-name> -o jsonpath='{.status.containerStatuses[?(@.name=="step-build")].state}' | jq
```

### Access Step Results

```bash
# View task results
tkn taskrun describe <taskrun-name> | grep -A 10 "Results"

# Get specific result
kubectl get taskrun <name> -o jsonpath='{.status.taskResults[?(@.name=="commit-sha")].value}'
```

## Useful Aliases and Functions

### Bash Aliases

```bash
# Add to ~/.bashrc

alias tkn-logs='tkn pipelinerun logs --last -f'
alias tkn-list='tkn pipelinerun list'
alias tkn-desc='tkn pipelinerun describe --last'
alias tkn-start='tkn pipeline start'
alias tkn-cancel='tkn pipelinerun cancel --last'
alias tkn-tasks='tkn taskrun list'
alias tkn-clean='tkn pipelinerun delete --all -f'
```

### Bash Functions

```bash
# Add to ~/.bashrc

# Follow logs for specific pipeline
tkn-watch() {
  tkn pipeline start "$1" --showlog
}

# Delete old pipelineruns
tkn-prune() {
  local keep=${1:-5}
  tkn pipelinerun delete --keep $keep -f
}

# Get pipeline status
tkn-status() {
  tkn pipelinerun list | head -n 10
}

# Restart last failed pipeline
tkn-retry() {
  local last_pr=$(tkn pipelinerun list -o name | head -n 1 | cut -d'/' -f2)
  tkn pipelinerun delete $last_pr -f
  tkn pipeline start $(tkn pipelinerun describe $last_pr -o jsonpath='{.spec.pipelineRef.name}') --last
}
```

## Common Workflows

### Start Pipeline with Full Options

```bash
tkn pipeline start build-deploy \
  -p git-url=https://github.com/myorg/myrepo \
  -p git-revision=main \
  -p image-name=myregistry.com/myapp:latest \
  -w name=shared-data,volumeClaimTemplateFile=pvc-template.yaml \
  --serviceaccount=pipeline-sa \
  --showlog
```

### Monitor Pipeline Execution

```bash
# Start and follow logs
tkn pipeline start my-pipeline --showlog

# Or in separate commands
tkn pipeline start my-pipeline
tkn pipelinerun logs --last -f
```

### Cleanup Old Runs

```bash
# Keep last 5 runs
tkn pipelinerun delete --keep 5 -f

# Delete runs older than 60 minutes
tkn pipelinerun delete --keep-since 60 -f

# Delete all failed runs
tkn pipelinerun delete --all --failed -f
```

### Export PipelineRun for Rerun

```bash
# Export last run as YAML
tkn pipelinerun describe --last -o yaml > rerun.yaml

# Edit if needed
vim rerun.yaml

# Remove status and metadata.name
yq eval 'del(.status) | del(.metadata.name) | del(.metadata.namespace) | del(.metadata.uid)' rerun.yaml > clean-rerun.yaml

# Apply
kubectl apply -f clean-rerun.yaml
```

## Troubleshooting

### tkn Command Not Found

```bash
# Check installation
which tkn

# Add to PATH if needed
export PATH=$PATH:/usr/local/bin

# Reinstall
brew reinstall tektoncd-cli  # macOS
```

### Cannot Connect to Cluster

```bash
# Check kubectl context
kubectl config current-context

# Check Tekton installation
kubectl get pods -n tekton-pipelines

# Check API access
kubectl api-resources | grep tekton
```

### Logs Not Showing

```bash
# Check pod exists
kubectl get pods | grep <pipelinerun-name>

# Check pod status
kubectl describe pod <pod-name>

# Try kubectl logs directly
kubectl logs <pod-name> -c step-<step-name>
```

### Permission Denied

```bash
# Check RBAC
kubectl auth can-i create pipelineruns

# Check ServiceAccount
kubectl get serviceaccount

# Check RoleBinding
kubectl get rolebinding | grep tekton
```

## Best Practices

- Always use `--showlog` or `-f` to follow pipeline execution
- Set up shell completion for faster command entry
- Use aliases for frequently used commands
- Clean up old pipelineruns regularly to free resources
- Use `--last` flag to avoid typing pipelinerun names
- Export successful runs as templates for reuse
- Monitor events during debugging (`kubectl get events -w`)
- Use `tkn hub` to discover and install community tasks
