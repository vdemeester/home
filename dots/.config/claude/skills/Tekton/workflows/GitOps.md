# GitOps Workflow

Integrating Tekton with GitOps tools (ArgoCD, Flux CD) for complete CI/CD automation.

## When to Use

- Implementing GitOps workflows
- Integrating Tekton with ArgoCD or Flux
- Automating deployments via Git
- Separating CI and CD concerns
- Managing multi-environment deployments

## GitOps Architecture

### Tekton + ArgoCD Pattern

```
┌──────────┐    ┌────────────┐    ┌──────────────┐    ┌─────────────┐
│  GitHub  │───▶│   Tekton   │───▶│  GitOps Repo │───▶│   ArgoCD    │
│  (Code)  │    │    (CI)    │    │  (Manifests) │    │    (CD)     │
└──────────┘    └────────────┘    └──────────────┘    └─────────────┘
                     │                                        │
                     │ Build & Push                           │ Sync
                     ▼                                        ▼
               ┌────────────┐                          ┌─────────────┐
               │  Registry  │                          │  Kubernetes │
               └────────────┘                          └─────────────┘
```

**Responsibilities:**
- **Tekton (CI)**: Build, test, create container images
- **ArgoCD (CD)**: Deploy manifests to Kubernetes

**Benefits:**
- Separation of concerns
- Git as single source of truth
- Declarative deployments
- Automated rollback capabilities
- Audit trail via Git history

## Tekton CI Pipeline for GitOps

### Complete CI Pipeline

```yaml
apiVersion: tekton.dev/v1
kind: Pipeline
metadata:
  name: gitops-ci-pipeline
spec:
  params:
    - name: git-url
      description: Application source repository
    - name: git-revision
      description: Git commit/branch
      default: main
    - name: image-name
      description: Container image name
    - name: gitops-repo-url
      description: GitOps repository URL
    - name: gitops-branch
      description: GitOps repository branch
      default: main
    - name: deployment-file
      description: Path to deployment file
      default: k8s/deployment.yaml

  workspaces:
    - name: source-code
    - name: gitops-repo
    - name: docker-credentials
    - name: git-credentials

  tasks:
    # 1. Clone application source
    - name: clone-source
      taskRef:
        resolver: hub
        params:
          - name: name
            value: git-clone
      workspaces:
        - name: output
          workspace: source-code
        - name: ssh-directory
          workspace: git-credentials
      params:
        - name: url
          value: $(params.git-url)
        - name: revision
          value: $(params.git-revision)

    # 2. Extract version from commit
    - name: get-version
      runAfter: [clone-source]
      taskRef:
        name: git-version
      workspaces:
        - name: source
          workspace: source-code

    # 3. Run tests
    - name: test
      runAfter: [clone-source]
      taskRef:
        name: run-tests
      workspaces:
        - name: source
          workspace: source-code

    # 4. Build and push image
    - name: build-push
      runAfter: [test, get-version]
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
        - name: EXTRA_ARGS
          value:
            - --cache=true

    # 5. Clone GitOps repository
    - name: clone-gitops
      runAfter: [build-push]
      taskRef:
        resolver: hub
        params:
          - name: name
            value: git-clone
      workspaces:
        - name: output
          workspace: gitops-repo
        - name: ssh-directory
          workspace: git-credentials
      params:
        - name: url
          value: $(params.gitops-repo-url)
        - name: revision
          value: $(params.gitops-branch)

    # 6. Update image tag in GitOps repo
    - name: update-manifest
      runAfter: [clone-gitops]
      taskRef:
        name: yq-update
      workspaces:
        - name: source
          workspace: gitops-repo
      params:
        - name: file
          value: $(params.deployment-file)
        - name: expression
          value: .spec.template.spec.containers[0].image = "$(params.image-name):$(tasks.get-version.results.version)"

    # 7. Commit and push changes
    - name: commit-push
      runAfter: [update-manifest]
      taskRef:
        name: git-cli
      workspaces:
        - name: source
          workspace: gitops-repo
        - name: ssh-directory
          workspace: git-credentials
      params:
        - name: GIT_USER_NAME
          value: "Tekton Pipeline"
        - name: GIT_USER_EMAIL
          value: "tekton@example.com"
        - name: GIT_SCRIPT
          value: |
            git add $(params.deployment-file)
            git commit -m "Update image to $(params.image-name):$(tasks.get-version.results.version)"
            git push origin $(params.gitops-branch)
```

### Update Manifest Task

```yaml
apiVersion: tekton.dev/v1
kind: Task
metadata:
  name: yq-update
spec:
  params:
    - name: file
      description: YAML file to update
    - name: expression
      description: yq expression
  workspaces:
    - name: source
  steps:
    - name: update
      image: mikefarah/yq:latest
      workingDir: $(workspaces.source.path)
      script: |
        #!/bin/sh
        yq eval -i '$(params.expression)' $(params.file)
```

### Git Commit Task

```yaml
apiVersion: tekton.dev/v1
kind: Task
metadata:
  name: git-commit-push
spec:
  params:
    - name: commit-message
    - name: file-path
  workspaces:
    - name: source
    - name: ssh-directory
  steps:
    - name: commit-push
      image: alpine/git:latest
      workingDir: $(workspaces.source.path)
      env:
        - name: GIT_SSH_COMMAND
          value: ssh -i $(workspaces.ssh-directory.path)/id_rsa -o StrictHostKeyChecking=no
      script: |
        #!/bin/sh
        set -e

        git config user.name "Tekton Pipeline"
        git config user.email "tekton@example.com"

        git add $(params.file-path)
        git commit -m "$(params.commit-message)"
        git push origin HEAD
```

## ArgoCD Integration

### ArgoCD Application

```yaml
apiVersion: argoproj.io/v1alpha1
kind: Application
metadata:
  name: myapp
  namespace: argocd
spec:
  project: default
  source:
    repoURL: https://github.com/org/gitops-repo
    targetRevision: main
    path: k8s
  destination:
    server: https://kubernetes.default.svc
    namespace: production
  syncPolicy:
    automated:
      prune: true
      selfHeal: true
    syncOptions:
      - CreateNamespace=true
```

### Trigger ArgoCD Sync from Tekton

```yaml
# Task to trigger ArgoCD sync
apiVersion: tekton.dev/v1
kind: Task
metadata:
  name: argocd-sync
spec:
  params:
    - name: app-name
      description: ArgoCD application name
    - name: argocd-server
      description: ArgoCD server URL
      default: argocd-server.argocd.svc:443
  steps:
    - name: sync
      image: argoproj/argocd:latest
      script: |
        #!/bin/sh
        argocd app sync $(params.app-name) \
          --server $(params.argocd-server) \
          --auth-token $ARGOCD_TOKEN
      env:
        - name: ARGOCD_TOKEN
          valueFrom:
            secretKeyRef:
              name: argocd-token
              key: token
```

## Flux CD Integration

### Flux ImageUpdateAutomation

**Flux automatically updates images:**

```yaml
apiVersion: image.toolkit.fluxcd.io/v1beta1
kind: ImageRepository
metadata:
  name: myapp
  namespace: flux-system
spec:
  image: myregistry.com/myapp
  interval: 1m
---
apiVersion: image.toolkit.fluxcd.io/v1beta1
kind: ImagePolicy
metadata:
  name: myapp
  namespace: flux-system
spec:
  imageRepositoryRef:
    name: myapp
  policy:
    semver:
      range: 1.x.x
---
apiVersion: image.toolkit.fluxcd.io/v1beta1
kind: ImageUpdateAutomation
metadata:
  name: myapp
  namespace: flux-system
spec:
  interval: 1m
  sourceRef:
    kind: GitRepository
    name: gitops-repo
  git:
    checkout:
      ref:
        branch: main
    commit:
      author:
        email: flux@example.com
        name: Flux
      messageTemplate: "Update image to {{range .Updated.Images}}{{println .}}{{end}}"
    push:
      branch: main
  update:
    path: ./k8s
    strategy: Setters
```

### Tekton with Flux

**Simpler Tekton pipeline:**
```yaml
# Tekton only builds and pushes
# Flux automatically detects new image and updates manifests
spec:
  tasks:
    - name: clone
    - name: test
    - name: build-push
  # No manifest update needed - Flux handles it
```

## Multi-Environment GitOps

### Directory Structure

```
gitops-repo/
├── base/
│   ├── deployment.yaml
│   ├── service.yaml
│   └── kustomization.yaml
├── overlays/
│   ├── dev/
│   │   ├── kustomization.yaml
│   │   └── patches/
│   ├── staging/
│   │   ├── kustomization.yaml
│   │   └── patches/
│   └── production/
│       ├── kustomization.yaml
│       └── patches/
```

### Kustomize Update Task

```yaml
apiVersion: tekton.dev/v1
kind: Task
metadata:
  name: kustomize-set-image
spec:
  params:
    - name: image
    - name: overlay
      default: dev
  workspaces:
    - name: source
  steps:
    - name: set-image
      image: k8s.gcr.io/kustomize/kustomize:v5.0.0
      workingDir: $(workspaces.source.path)/overlays/$(params.overlay)
      script: |
        kustomize edit set image app=$(params.image)
```

### Environment-Specific Pipelines

```yaml
# Dev pipeline - auto-deploy
- name: update-dev
  when:
    - input: $(params.git-branch)
      operator: in
      values: [develop]
  taskRef:
    name: update-gitops
  params:
    - name: overlay
      value: dev

# Staging pipeline - auto-deploy
- name: update-staging
  when:
    - input: $(params.git-branch)
      operator: in
      values: [main]
  taskRef:
    name: update-gitops
  params:
    - name: overlay
      value: staging

# Production - manual approval required
- name: update-production
  when:
    - input: $(params.deploy-to-prod)
      operator: in
      values: ["true"]
  taskRef:
    name: update-gitops
  params:
    - name: overlay
      value: production
```

## Git Credentials Management

### SSH Key Secret

```bash
# Generate SSH key
ssh-keygen -t ed25519 -C "tekton@example.com" -f tekton_key -N ""

# Create secret
kubectl create secret generic git-ssh-credentials \
  --from-file=id_rsa=tekton_key \
  --from-file=id_rsa.pub=tekton_key.pub \
  --from-file=known_hosts=~/.ssh/known_hosts

# Add public key to GitHub/GitLab as deploy key with write access
```

### Using SSH Credentials

```yaml
workspaces:
  - name: ssh-directory
    secret:
      secretName: git-ssh-credentials
      items:
        - key: id_rsa
          path: id_rsa
          mode: 0600
        - key: known_hosts
          path: known_hosts
```

## Best Practices

### Repository Structure
- Separate application and GitOps repositories
- Use Kustomize or Helm for multi-environment management
- Version GitOps manifests
- Protect main branch with pull requests

### CI Pipeline
- Build and test in Tekton
- Push images with semantic versioning
- Update GitOps repo via automation
- Sign commits for traceability

### CD with ArgoCD/Flux
- Enable auto-sync for development
- Require manual approval for production
- Configure sync windows for production
- Set up health checks and rollback

### Security
- Use SSH keys with write-only access to GitOps repo
- Rotate credentials regularly
- Scan container images before pushing
- Use signed commits

### Monitoring
- Monitor ArgoCD sync status
- Alert on sync failures
- Track deployment frequency
- Measure lead time for changes

## Complete GitOps Example

```yaml
# PipelineRun with all parameters
apiVersion: tekton.dev/v1
kind: PipelineRun
metadata:
  generateName: gitops-ci-run-
spec:
  pipelineRef:
    name: gitops-ci-pipeline
  params:
    - name: git-url
      value: https://github.com/org/myapp
    - name: git-revision
      value: main
    - name: image-name
      value: myregistry.com/myapp
    - name: gitops-repo-url
      value: git@github.com:org/gitops-repo.git
    - name: gitops-branch
      value: main
    - name: deployment-file
      value: overlays/production/kustomization.yaml
  workspaces:
    - name: source-code
      volumeClaimTemplate:
        spec:
          accessModes:
            - ReadWriteOnce
          resources:
            requests:
              storage: 1Gi
    - name: gitops-repo
      volumeClaimTemplate:
        spec:
          accessModes:
            - ReadWriteOnce
          resources:
            requests:
              storage: 500Mi
    - name: docker-credentials
      secret:
        secretName: docker-config
    - name: git-credentials
      secret:
        secretName: git-ssh-credentials
  serviceAccountName: gitops-pipeline-sa
```

## Troubleshooting

### Manifest Update Fails

```bash
# Check workspace contents
kubectl exec <pod-name> -- ls /workspace/gitops-repo

# Verify yq/kustomize syntax
kubectl logs <pod-name> -c step-update
```

### Git Push Fails

```bash
# Check SSH credentials
kubectl get secret git-ssh-credentials -o yaml

# Test SSH connection
kubectl run test-git --rm -i --tty --image=alpine/git -- sh
apk add openssh-client
ssh -T git@github.com
```

### ArgoCD Not Syncing

```bash
# Check ArgoCD application
kubectl get application myapp -n argocd

# View sync status
argocd app get myapp

# Manual sync
argocd app sync myapp
```

### Wrong Image Tag

```bash
# Check task results
kubectl get pipelinerun <name> -o jsonpath='{.status.taskRuns}'

# Verify manifest update
kubectl logs <pod-name> -c step-update
```
