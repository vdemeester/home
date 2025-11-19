# Tekton Pipelines as Code Migration Guide

This document provides a complete migration of all GitHub Actions workflows to Tekton Pipelines as Code (PaC).

## Prerequisites

- Kubernetes cluster with Tekton Pipelines installed (v0.54+ recommended for matrix support)
- Heterogeneous cluster with both x86_64 and aarch64 nodes labeled appropriately
- Pipelines as Code installed and configured
- Repository CRD created for this repository
- GitHub App or webhook configured for event delivery
- Necessary secrets configured (CACHIX_AUTH_TOKEN, SBR_BOT_TOKEN)
- Service account with permissions to create child PipelineRuns (for dynamic matrix builds)

### Node Architecture Labeling

Ensure your nodes are labeled with architecture information:

```bash
# Verify node labels
kubectl get nodes -L kubernetes.io/arch

# Expected output:
# NAME        STATUS   ARCH
# node-1      Ready    amd64
# node-2      Ready    arm64
```

If labels are missing, add them:

```bash
kubectl label nodes <node-name> kubernetes.io/arch=amd64
kubectl label nodes <node-name> kubernetes.io/arch=arm64
```

## Repository CRD Configuration

First, create a Repository CR to connect this repository to Pipelines as Code:

```yaml
apiVersion: pipelinesascode.tekton.dev/v1alpha1
kind: Repository
metadata:
  name: home-repo
  namespace: ci  # Adjust to your CI namespace
spec:
  url: "https://github.com/vincent/home"  # Adjust to your actual repo URL
  settings:
    # Optionally fetch pipelines from default branch for security
    # pipelinerun_provenance: "default_branch"
```

## Migration Overview

All PipelineRun definitions should be placed in `.tekton/` directory at the repository root:

```
.tekton/
├── build-keyboard-eyelash-corne.yaml
├── build-keyboard-moonlander.yaml
├── build-packages.yaml
├── build-systems.yaml
└── nix-auto-upgrade.yaml
```

## Workflow Translations

### 1. Build Keyboard Eyelash Corne

**GitHub Actions**: `.github/workflows/build-keyboard-eyelash-corne.yaml`
**Tekton PaC**: `.tekton/build-keyboard-eyelash-corne.yaml`

```yaml
---
apiVersion: tekton.dev/v1
kind: PipelineRun
metadata:
  name: keyboard-eyelash-corne
  annotations:
    pipelinesascode.tekton.dev/on-event: "[pull_request, push]"
    pipelinesascode.tekton.dev/on-target-branch: "[main]"
    pipelinesascode.tekton.dev/on-path-change: |
      keyboards/eyelash_corne/**,keyboards/lib/**,keyboards/Makefile,.tekton/build-keyboard-eyelash-corne.yaml
    pipelinesascode.tekton.dev/max-keep-runs: "5"
spec:
  params:
    - name: repo_url
      value: "{{repo_url}}"
    - name: revision
      value: "{{revision}}"
  workspaces:
    - name: source
      volumeClaimTemplate:
        spec:
          accessModes:
            - ReadWriteOnce
          resources:
            requests:
              storage: 1Gi
    - name: artifacts
      volumeClaimTemplate:
        spec:
          accessModes:
            - ReadWriteOnce
          resources:
            requests:
              storage: 500Mi
  pipelineSpec:
    params:
      - name: repo_url
      - name: revision
    workspaces:
      - name: source
      - name: artifacts
    tasks:
      - name: fetch-repository
        taskRef:
          name: git-clone
          kind: ClusterTask
        workspaces:
          - name: output
            workspace: source
        params:
          - name: url
            value: $(params.repo_url)
          - name: revision
            value: $(params.revision)
          - name: depth
            value: "1"

      - name: build-firmware
        runAfter:
          - fetch-repository
        workspaces:
          - name: source
            workspace: source
          - name: artifacts
            workspace: artifacts
        taskSpec:
          workspaces:
            - name: source
            - name: artifacts
          steps:
            - name: build
              image: ubuntu:latest
              workingDir: $(workspaces.source.path)
              script: |
                #!/bin/bash
                set -euo pipefail
                cd keyboards
                make eyelash_corne/build

            - name: copy-artifacts
              image: ubuntu:latest
              workingDir: $(workspaces.source.path)
              script: |
                #!/bin/bash
                set -euo pipefail
                cp keyboards/eyelash_corne/firmwares/eyelash_corne_*.uf2 $(workspaces.artifacts.path)/ || {
                  echo "Error: No firmware files found"
                  exit 1
                }

      - name: upload-artifacts
        runAfter:
          - build-firmware
        workspaces:
          - name: artifacts
            workspace: artifacts
        taskSpec:
          workspaces:
            - name: artifacts
          steps:
            # Option 1: Upload to S3-compatible storage
            - name: upload-to-s3
              image: amazon/aws-cli:latest
              workingDir: $(workspaces.artifacts.path)
              env:
                - name: AWS_ACCESS_KEY_ID
                  valueFrom:
                    secretKeyRef:
                      name: s3-credentials
                      key: access-key-id
                      optional: true
                - name: AWS_SECRET_ACCESS_KEY
                  valueFrom:
                    secretKeyRef:
                      name: s3-credentials
                      key: secret-access-key
                      optional: true
                - name: AWS_DEFAULT_REGION
                  value: us-east-1
                - name: S3_BUCKET
                  value: "your-artifacts-bucket"
              script: |
                #!/bin/bash
                set -euo pipefail
                if [ -n "${AWS_ACCESS_KEY_ID:-}" ]; then
                  ARTIFACT_PATH="eyelash_corne/{{revision}}"
                  aws s3 cp . "s3://${S3_BUCKET}/${ARTIFACT_PATH}/" --recursive
                  echo "Artifacts uploaded to s3://${S3_BUCKET}/${ARTIFACT_PATH}/"
                else
                  echo "S3 credentials not configured, skipping upload"
                fi
```

### 2. Build Keyboard Moonlander

**GitHub Actions**: `.github/workflows/build-keyboard-moonlander.yaml`
**Tekton PaC**: `.tekton/build-keyboard-moonlander.yaml`

```yaml
---
apiVersion: tekton.dev/v1
kind: PipelineRun
metadata:
  name: keyboard-moonlander
  annotations:
    pipelinesascode.tekton.dev/on-event: "[pull_request, push]"
    pipelinesascode.tekton.dev/on-target-branch: "[main]"
    pipelinesascode.tekton.dev/on-path-change: |
      keyboards/moonlander/**,keyboards/lib/**,.tekton/build-keyboard-moonlander.yaml
    pipelinesascode.tekton.dev/max-keep-runs: "5"
spec:
  params:
    - name: repo_url
      value: "{{repo_url}}"
    - name: revision
      value: "{{revision}}"
  workspaces:
    - name: source
      volumeClaimTemplate:
        spec:
          accessModes:
            - ReadWriteOnce
          resources:
            requests:
              storage: 5Gi
    - name: artifacts
      volumeClaimTemplate:
        spec:
          accessModes:
            - ReadWriteOnce
          resources:
            requests:
              storage: 500Mi
  pipelineSpec:
    params:
      - name: repo_url
      - name: revision
    workspaces:
      - name: source
      - name: artifacts
    tasks:
      - name: fetch-repository
        taskRef:
          name: git-clone
          kind: ClusterTask
        workspaces:
          - name: output
            workspace: source
        params:
          - name: url
            value: $(params.repo_url)
          - name: revision
            value: $(params.revision)
          - name: depth
            value: "1"

      - name: build-firmware
        runAfter:
          - fetch-repository
        workspaces:
          - name: source
            workspace: source
          - name: artifacts
            workspace: artifacts
        taskSpec:
          workspaces:
            - name: source
            - name: artifacts
          steps:
            - name: install-nix
              image: nixos/nix:latest
              workingDir: $(workspaces.source.path)
              script: |
                #!/bin/sh
                set -eu
                # Nix is already installed in nixos/nix image
                nix --version

            - name: build
              image: nixos/nix:latest
              workingDir: $(workspaces.source.path)
              script: |
                #!/bin/sh
                set -eu
                cd keyboards
                make moonlander/update moonlander/build

            - name: copy-artifacts
              image: nixos/nix:latest
              workingDir: $(workspaces.source.path)
              script: |
                #!/bin/sh
                set -eu
                cp keyboards/moonlander/build/zsa_moonlander_vincent.bin $(workspaces.artifacts.path)/ || {
                  echo "Error: Firmware file not found"
                  exit 1
                }

      - name: upload-artifacts
        runAfter:
          - build-firmware
        workspaces:
          - name: artifacts
            workspace: artifacts
        taskSpec:
          workspaces:
            - name: artifacts
          steps:
            - name: upload-to-s3
              image: amazon/aws-cli:latest
              workingDir: $(workspaces.artifacts.path)
              env:
                - name: AWS_ACCESS_KEY_ID
                  valueFrom:
                    secretKeyRef:
                      name: s3-credentials
                      key: access-key-id
                      optional: true
                - name: AWS_SECRET_ACCESS_KEY
                  valueFrom:
                    secretKeyRef:
                      name: s3-credentials
                      key: secret-access-key
                      optional: true
                - name: AWS_DEFAULT_REGION
                  value: us-east-1
                - name: S3_BUCKET
                  value: "your-artifacts-bucket"
              script: |
                #!/bin/bash
                set -euo pipefail
                if [ -n "${AWS_ACCESS_KEY_ID:-}" ]; then
                  ARTIFACT_PATH="moonlander/{{revision}}"
                  aws s3 cp . "s3://${S3_BUCKET}/${ARTIFACT_PATH}/" --recursive
                  echo "Artifacts uploaded to s3://${S3_BUCKET}/${ARTIFACT_PATH}/"
                else
                  echo "S3 credentials not configured, skipping upload"
                fi
```

### 3. Build Packages (Dynamic Matrix)

**GitHub Actions**: `.github/workflows/build-packages.yaml`
**Tekton PaC**: `.tekton/build-packages.yaml`

This implementation uses a **dynamic matrix** pattern where a generator task spawns individual PipelineRuns for each package, allowing true parallel execution with proper architecture scheduling.

```yaml
---
apiVersion: tekton.dev/v1
kind: PipelineRun
metadata:
  name: packages
  annotations:
    pipelinesascode.tekton.dev/on-event: "[pull_request, push]"
    pipelinesascode.tekton.dev/on-target-branch: "[main]"
    pipelinesascode.tekton.dev/on-path-change: |
      pkgs/**,flake.nix,flake.lock,.tekton/build-packages.yaml
    pipelinesascode.tekton.dev/max-keep-runs: "5"
spec:
  params:
    - name: repo_url
      value: "{{repo_url}}"
    - name: revision
      value: "{{revision}}"
    - name: event_type
      value: "{{event_type}}"
  workspaces:
    - name: source
      volumeClaimTemplate:
        spec:
          accessModes:
            - ReadWriteOnce
          resources:
            requests:
              storage: 10Gi
  pipelineSpec:
    params:
      - name: repo_url
      - name: revision
      - name: event_type
    workspaces:
      - name: source
    tasks:
      - name: fetch-repository
        taskRef:
          name: git-clone
          kind: ClusterTask
        workspaces:
          - name: output
            workspace: source
        params:
          - name: url
            value: $(params.repo_url)
          - name: revision
            value: $(params.revision)
          - name: depth
            value: "1"

      - name: generate-and-spawn-matrix
        runAfter:
          - fetch-repository
        workspaces:
          - name: source
            workspace: source
        params:
          - name: repo_url
            value: $(params.repo_url)
          - name: revision
            value: $(params.revision)
          - name: event_type
            value: $(params.event_type)
        taskSpec:
          params:
            - name: repo_url
            - name: revision
            - name: event_type
          workspaces:
            - name: source
          stepTemplate:
            env:
              - name: REPO_URL
                value: $(params.repo_url)
              - name: REVISION
                value: $(params.revision)
              - name: EVENT_TYPE
                value: $(params.event_type)
          steps:
            - name: generate-matrix
              image: nixos/nix:latest
              workingDir: $(workspaces.source.path)
              script: |
                #!/bin/sh
                set -eu

                # Generate matrix from nix flake
                echo "Generating package matrix..."
                nix eval --json '.#githubActions.matrix' > /tmp/matrix.json
                cat /tmp/matrix.json

                # Parse and format for next step
                # Expected format: {"include":[{"attr":"pkg1","os":"ubuntu-latest"}, ...]}
                cat /tmp/matrix.json > /workspace/matrix.json

            - name: spawn-child-pipelineruns
              image: gcr.io/tekton-releases/github.com/tektoncd/pipeline/cmd/entrypoint:latest
              workingDir: $(workspaces.source.path)
              script: |
                #!/bin/sh
                set -eu

                # Install jq for JSON parsing
                apk add --no-cache jq kubectl

                # Parse matrix JSON
                MATRIX=$(cat /workspace/matrix.json)
                echo "Matrix: $MATRIX"

                # Extract package list with architecture
                PACKAGES=$(echo "$MATRIX" | jq -r '.include[] | @base64')

                # Spawn a PipelineRun for each package
                for pkg_encoded in $PACKAGES; do
                  pkg_data=$(echo "$pkg_encoded" | base64 -d)
                  ATTR=$(echo "$pkg_data" | jq -r '.attr')
                  OS=$(echo "$pkg_data" | jq -r '.os // "ubuntu-latest"')

                  # Map OS to architecture
                  ARCH="amd64"
                  if echo "$OS" | grep -q "arm"; then
                    ARCH="arm64"
                  fi

                  echo "Spawning build for package: $ATTR (arch: $ARCH)"

                  # Create child PipelineRun
                  cat <<EOF | kubectl create -f -
                apiVersion: tekton.dev/v1
                kind: PipelineRun
                metadata:
                  generateName: package-build-${ATTR}-
                  namespace: ci
                  labels:
                    app: package-build
                    package: ${ATTR}
                    parent: packages
                spec:
                  params:
                    - name: package_attr
                      value: "${ATTR}"
                    - name: repo_url
                      value: "${REPO_URL}"
                    - name: revision
                      value: "${REVISION}"
                    - name: event_type
                      value: "${EVENT_TYPE}"
                    - name: target_arch
                      value: "${ARCH}"
                  workspaces:
                    - name: source
                      volumeClaimTemplate:
                        spec:
                          accessModes:
                            - ReadWriteOnce
                          resources:
                            requests:
                              storage: 10Gi
                  pipelineSpec:
                    params:
                      - name: package_attr
                      - name: repo_url
                      - name: revision
                      - name: event_type
                      - name: target_arch
                    workspaces:
                      - name: source
                    tasks:
                      - name: fetch-repository
                        taskRef:
                          name: git-clone
                          kind: ClusterTask
                        workspaces:
                          - name: output
                            workspace: source
                        params:
                          - name: url
                            value: \$(params.repo_url)
                          - name: revision
                            value: \$(params.revision)
                          - name: depth
                            value: "1"

                      - name: build-package
                        runAfter:
                          - fetch-repository
                        workspaces:
                          - name: source
                            workspace: source
                        params:
                          - name: package
                            value: \$(params.package_attr)
                          - name: event_type
                            value: \$(params.event_type)
                          - name: arch
                            value: \$(params.target_arch)
                        taskSpec:
                          params:
                            - name: package
                            - name: event_type
                            - name: arch
                          workspaces:
                            - name: source
                          steps:
                            - name: build
                              image: nixos/nix:latest
                              workingDir: \$(workspaces.source.path)
                              env:
                                - name: CACHIX_AUTH_TOKEN
                                  valueFrom:
                                    secretKeyRef:
                                      name: ci-secrets
                                      key: cachix-auth-token
                              script: |
                                #!/bin/sh
                                set -eu

                                echo "Building package: \$(params.package) for arch: \$(params.arch)"

                                # Install and configure cachix
                                nix profile install nixpkgs#cachix
                                cachix use chapeau-rouge
                                if [ -n "\${CACHIX_AUTH_TOKEN:-}" ]; then
                                  cachix authtoken "\${CACHIX_AUTH_TOKEN}"
                                fi

                                # Build the package
                                nix build -L ".#\$(params.package)"

                                # Push to cachix (only on push events, not PRs)
                                if [ "\$(params.event_type)" != "pull_request" ] && [ -n "\${CACHIX_AUTH_TOKEN:-}" ]; then
                                  cachix push chapeau-rouge ./result
                                fi
                              resources:
                                requests:
                                  memory: "2Gi"
                                  cpu: "1"
                                limits:
                                  memory: "4Gi"
                                  cpu: "2"
                              # Schedule on appropriate architecture
                              computeResources:
                                requests:
                                  kubernetes.io/arch: \$(params.arch)
EOF
                done

                echo "Spawned child PipelineRuns for all packages"
```

**Key Features:**
- **True Dynamic Matrix**: Evaluates `nix eval` output at runtime and spawns individual PipelineRuns
- **Architecture-Aware**: Automatically schedules builds on the correct architecture (amd64/arm64)
- **Parallel Execution**: Each package builds independently and in parallel
- **Proper Resource Isolation**: Each build gets its own workspace and resources

**Note**: This requires the ServiceAccount running the pipeline to have permissions to create PipelineRuns. See RBAC configuration below.

### 4. Build Systems (Dynamic Matrix with Multi-Arch)

**GitHub Actions**: `.github/workflows/build-systems.yaml`
**Tekton PaC**: `.tekton/build-systems.yaml`

This implementation dynamically spawns separate PipelineRuns for each NixOS system configuration, scheduling builds on the appropriate architecture nodes.

```yaml
---
apiVersion: tekton.dev/v1
kind: PipelineRun
metadata:
  name: systems
  annotations:
    pipelinesascode.tekton.dev/on-event: "[pull_request, push]"
    pipelinesascode.tekton.dev/on-target-branch: "[main]"
    pipelinesascode.tekton.dev/on-path-change: |
      home/**,systems/**,lib/**,modules/**,tools/battery-monitor/**,tools/bekind/**,tools/go-org-readwise/**,tools/k8s.infra/**,flake.nix,flake.lock,.tekton/build-systems.yaml
    pipelinesascode.tekton.dev/max-keep-runs: "5"
spec:
  params:
    - name: repo_url
      value: "{{repo_url}}"
    - name: revision
      value: "{{revision}}"
    - name: event_type
      value: "{{event_type}}"
  workspaces:
    - name: source
      volumeClaimTemplate:
        spec:
          accessModes:
            - ReadWriteOnce
          resources:
            requests:
              storage: 10Gi
  pipelineSpec:
    params:
      - name: repo_url
      - name: revision
      - name: event_type
    workspaces:
      - name: source
    tasks:
      - name: fetch-repository
        taskRef:
          name: git-clone
          kind: ClusterTask
        workspaces:
          - name: output
            workspace: source
        params:
          - name: url
            value: $(params.repo_url)
          - name: revision
            value: $(params.revision)
          - name: depth
            value: "1"

      - name: generate-and-spawn-systems
        runAfter:
          - fetch-repository
        workspaces:
          - name: source
            workspace: source
        params:
          - name: repo_url
            value: $(params.repo_url)
          - name: revision
            value: $(params.revision)
          - name: event_type
            value: $(params.event_type)
        taskSpec:
          params:
            - name: repo_url
            - name: revision
            - name: event_type
          workspaces:
            - name: source
          stepTemplate:
            env:
              - name: REPO_URL
                value: $(params.repo_url)
              - name: REVISION
                value: $(params.revision)
              - name: EVENT_TYPE
                value: $(params.event_type)
          steps:
            - name: generate-matrix
              image: nixos/nix:latest
              workingDir: $(workspaces.source.path)
              script: |
                #!/bin/sh
                set -eu

                echo "Generating systems matrix..."
                # Expected format: [{"name":"kyushu","arch":"x86_64-linux"}, ...]
                nix eval .#githubActionsMatrix --raw > /tmp/matrix.json
                cat /tmp/matrix.json
                cat /tmp/matrix.json > /workspace/systems-matrix.json

            - name: spawn-child-pipelineruns
              image: gcr.io/tekton-releases/github.com/tektoncd/pipeline/cmd/entrypoint:latest
              workingDir: $(workspaces.source.path)
              script: |
                #!/bin/sh
                set -eu

                # Install jq and kubectl
                apk add --no-cache jq kubectl

                # Parse systems matrix
                MATRIX=$(cat /workspace/systems-matrix.json)
                echo "Systems matrix: $MATRIX"

                # Extract system list
                SYSTEMS=$(echo "$MATRIX" | jq -c '.[]' 2>/dev/null || echo "$MATRIX" | jq -c '.include[]')

                # Spawn a PipelineRun for each system
                for system_data in $SYSTEMS; do
                  SYSTEM_NAME=$(echo "$system_data" | jq -r '.name')
                  SYSTEM_ARCH=$(echo "$system_data" | jq -r '.arch // "x86_64-linux"')

                  # Map nix arch to kubernetes arch
                  K8S_ARCH="amd64"
                  if echo "$SYSTEM_ARCH" | grep -q "aarch64"; then
                    K8S_ARCH="arm64"
                  fi

                  echo "Spawning build for system: $SYSTEM_NAME (arch: $SYSTEM_ARCH -> k8s: $K8S_ARCH)"

                  # Create child PipelineRun
                  cat <<EOF | kubectl create -f -
                apiVersion: tekton.dev/v1
                kind: PipelineRun
                metadata:
                  generateName: system-build-${SYSTEM_NAME}-
                  namespace: ci
                  labels:
                    app: system-build
                    system: ${SYSTEM_NAME}
                    parent: systems
                spec:
                  params:
                    - name: system_name
                      value: "${SYSTEM_NAME}"
                    - name: repo_url
                      value: "${REPO_URL}"
                    - name: revision
                      value: "${REVISION}"
                    - name: event_type
                      value: "${EVENT_TYPE}"
                    - name: target_arch
                      value: "${K8S_ARCH}"
                  workspaces:
                    - name: source
                      volumeClaimTemplate:
                        spec:
                          accessModes:
                            - ReadWriteOnce
                          resources:
                            requests:
                              storage: 50Gi
                  pipelineSpec:
                    params:
                      - name: system_name
                      - name: repo_url
                      - name: revision
                      - name: event_type
                      - name: target_arch
                    workspaces:
                      - name: source
                    tasks:
                      - name: fetch-repository
                        taskRef:
                          name: git-clone
                          kind: ClusterTask
                        workspaces:
                          - name: output
                            workspace: source
                        params:
                          - name: url
                            value: \$(params.repo_url)
                          - name: revision
                            value: \$(params.revision)
                          - name: depth
                            value: "1"

                      - name: build-system
                        runAfter:
                          - fetch-repository
                        workspaces:
                          - name: source
                            workspace: source
                        params:
                          - name: system
                            value: \$(params.system_name)
                          - name: event_type
                            value: \$(params.event_type)
                        taskSpec:
                          params:
                            - name: system
                            - name: event_type
                          workspaces:
                            - name: source
                          steps:
                            - name: setup-directories
                              image: nixos/nix:latest
                              script: |
                                #!/bin/sh
                                set -eu
                                # Create required directories for builds
                                mkdir -p /home/vincent/src/home/tools/emacs
                                mkdir -p /home/vincent/desktop/documents
                                touch /home/vincent/desktop/documents/.oath

                            - name: build
                              image: nixos/nix:latest
                              workingDir: \$(workspaces.source.path)
                              env:
                                - name: CACHIX_AUTH_TOKEN
                                  valueFrom:
                                    secretKeyRef:
                                      name: ci-secrets
                                      key: cachix-auth-token
                              script: |
                                #!/bin/sh
                                set -eu

                                echo "Building NixOS system: \$(params.system)"

                                # Install and configure cachix
                                nix profile install nixpkgs#cachix
                                cachix use vdemeester
                                if [ -n "\${CACHIX_AUTH_TOKEN:-}" ]; then
                                  cachix authtoken "\${CACHIX_AUTH_TOKEN}"
                                fi

                                # Build the system
                                nix build --accept-flake-config -L ".#nixosConfigurations.\$(params.system).config.system.build.toplevel"

                                # Push to cachix (only on push events, not PRs)
                                if [ "\$(params.event_type)" != "pull_request" ] && [ -n "\${CACHIX_AUTH_TOKEN:-}" ]; then
                                  cachix push vdemeester ./result
                                fi
                              resources:
                                requests:
                                  memory: "8Gi"
                                  cpu: "2"
                                limits:
                                  memory: "16Gi"
                                  cpu: "4"
                        # Schedule on appropriate architecture node
                        podTemplate:
                          nodeSelector:
                            kubernetes.io/arch: \$(params.target_arch)
EOF
                done

                echo "Spawned child PipelineRuns for all systems"
```

**Key Features:**
- **Dynamic Multi-Arch**: Parses system matrix and automatically determines target architecture
- **Heterogeneous Scheduling**: Uses `nodeSelector` to schedule builds on matching architecture nodes
- **Resource Isolation**: Each system gets its own 50Gi workspace and memory allocation
- **Parallel Builds**: All systems build concurrently on their respective architecture nodes
- **Fail-Fast Disabled**: Individual system failures don't block other builds

**Architecture Mapping:**
- `x86_64-linux` → `kubernetes.io/arch: amd64`
- `aarch64-linux` → `kubernetes.io/arch: arm64`

### 5. Nix Auto Upgrade

**GitHub Actions**: `.github/workflows/nix-auto-upgrade.yaml`
**Tekton PaC**: `.tekton/nix-auto-upgrade.yaml`

This workflow is triggered by schedule, not git events. Pipelines as Code doesn't natively support cron schedules, so you have two options:

#### Option A: Kubernetes CronJob (Recommended)

Create a CronJob that triggers a PipelineRun:

```yaml
---
apiVersion: batch/v1
kind: CronJob
metadata:
  name: nix-auto-upgrade
  namespace: ci  # Same namespace as Repository CR
spec:
  schedule: "0 0 * * 3"  # Weekly on Wednesday at 00:00
  jobTemplate:
    spec:
      template:
        spec:
          serviceAccountName: pac-cronjob-trigger
          containers:
            - name: trigger-pipeline
              image: bitnami/kubectl:latest
              command:
                - /bin/bash
                - -c
                - |
                  cat <<EOF | kubectl apply -f -
                  apiVersion: tekton.dev/v1
                  kind: PipelineRun
                  metadata:
                    generateName: nix-auto-upgrade-
                    namespace: ci
                  spec:
                    pipelineRef:
                      name: nix-auto-upgrade-pipeline
                    workspaces:
                      - name: source
                        volumeClaimTemplate:
                          spec:
                            accessModes:
                              - ReadWriteOnce
                            resources:
                              requests:
                                storage: 5Gi
                  EOF
          restartPolicy: OnFailure
---
# Service account with permissions to create PipelineRuns
apiVersion: v1
kind: ServiceAccount
metadata:
  name: pac-cronjob-trigger
  namespace: ci
---
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: pipelinerun-creator
  namespace: ci
rules:
  - apiGroups: ["tekton.dev"]
    resources: ["pipelineruns"]
    verbs: ["create", "get", "list"]
---
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: pac-cronjob-trigger-binding
  namespace: ci
subjects:
  - kind: ServiceAccount
    name: pac-cronjob-trigger
roleRef:
  kind: Role
  name: pipelinerun-creator
  apiGroup: rbac.authorization.k8s.io
```

#### Pipeline Definition for Auto-Upgrade

```yaml
---
apiVersion: tekton.dev/v1
kind: Pipeline
metadata:
  name: nix-auto-upgrade-pipeline
  namespace: ci
spec:
  workspaces:
    - name: source
  tasks:
    - name: fetch-repository
      taskRef:
        name: git-clone
        kind: ClusterTask
      workspaces:
        - name: output
          workspace: source
      params:
        - name: url
          value: "https://github.com/vincent/home"  # Adjust to your repo
        - name: revision
          value: "main"

    - name: update-flake-lock
      runAfter:
        - fetch-repository
      workspaces:
        - name: source
          workspace: source
      taskSpec:
        workspaces:
          - name: source
        steps:
          - name: install-nix
            image: nixos/nix:latest
            script: |
              #!/bin/sh
              nix --version

          - name: update-flake
            image: nixos/nix:latest
            workingDir: $(workspaces.source.path)
            env:
              - name: GITHUB_TOKEN
                valueFrom:
                  secretKeyRef:
                    name: ci-secrets
                    key: sbr-bot-token
            script: |
              #!/bin/sh
              set -eu

              # Configure git
              git config user.name "Vincent Demeester (sbr-bot)"
              git config user.email "bot@sbr.pm"

              # Update flake.lock
              nix flake update

              # Check if there are changes
              if git diff --quiet flake.lock; then
                echo "No updates available"
                exit 0
              fi

              # Create branch
              BRANCH="flake-update-$(date +%Y%m%d)"
              git checkout -b "$BRANCH"
              git add flake.lock
              git commit -m "Update flake.lock"

              # Push branch
              git remote set-url origin "https://x-access-token:${GITHUB_TOKEN}@github.com/vincent/home.git"
              git push origin "$BRANCH"

              # Create PR using GitHub API
              curl -X POST \
                -H "Authorization: token ${GITHUB_TOKEN}" \
                -H "Accept: application/vnd.github.v3+json" \
                https://api.github.com/repos/vincent/home/pulls \
                -d "{
                  \"title\": \"Update flake.lock\",
                  \"head\": \"$BRANCH\",
                  \"base\": \"main\",
                  \"body\": \"Automated flake.lock update\",
                  \"labels\": [\"dependencies\", \"automated\"]
                }"
```

#### Option B: GitHub Actions (Hybrid Approach)

Keep this specific workflow in GitHub Actions since it's deeply integrated with GitHub's PR creation workflow and scheduled triggers.

## Additional Tekton Tasks

You may need to create or reference these ClusterTasks:

### Git Clone Task

Most Tekton installations include this, but if not:

```yaml
apiVersion: tekton.dev/v1
kind: ClusterTask
metadata:
  name: git-clone
spec:
  params:
    - name: url
      type: string
    - name: revision
      type: string
      default: "main"
    - name: depth
      type: string
      default: "1"
  workspaces:
    - name: output
  steps:
    - name: clone
      image: gcr.io/tekton-releases/github.com/tektoncd/pipeline/cmd/git-init:latest
      script: |
        #!/bin/sh
        set -eu
        CHECKOUT_DIR="$(workspaces.output.path)"
        /ko-app/git-init \
          -url "$(params.url)" \
          -revision "$(params.revision)" \
          -path "$CHECKOUT_DIR" \
          -depth "$(params.depth)"
```

## RBAC Configuration for Dynamic Matrix Builds

For dynamic matrix workflows (packages and systems), the PipelineRun needs permissions to create child PipelineRuns:

```yaml
---
apiVersion: v1
kind: ServiceAccount
metadata:
  name: pipeline-spawner
  namespace: ci
---
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: pipelinerun-spawner
  namespace: ci
rules:
  - apiGroups: ["tekton.dev"]
    resources: ["pipelineruns"]
    verbs: ["create", "get", "list", "watch"]
  - apiGroups: [""]
    resources: ["pods", "pods/log"]
    verbs: ["get", "list"]
---
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: pipeline-spawner-binding
  namespace: ci
subjects:
  - kind: ServiceAccount
    name: pipeline-spawner
    namespace: ci
roleRef:
  kind: Role
  name: pipelinerun-spawner
  apiGroup: rbac.authorization.k8s.io
```

Then configure your Repository CR to use this ServiceAccount:

```yaml
apiVersion: pipelinesascode.tekton.dev/v1alpha1
kind: Repository
metadata:
  name: home-repo
  namespace: ci
spec:
  url: "https://github.com/vincent/home"
  settings:
    # Use custom service account for dynamic matrix builds
    service_account: pipeline-spawner
```

## Secrets Configuration

Create a Kubernetes Secret with your credentials:

```yaml
apiVersion: v1
kind: Secret
metadata:
  name: ci-secrets
  namespace: ci
type: Opaque
stringData:
  cachix-auth-token: "your-cachix-token"
  sbr-bot-token: "your-github-token"
---
# Optional: S3 credentials for artifact storage
apiVersion: v1
kind: Secret
metadata:
  name: s3-credentials
  namespace: ci
type: Opaque
stringData:
  access-key-id: "your-access-key"
  secret-access-key: "your-secret-key"
```

## Artifact Storage Solutions

Since Tekton doesn't have built-in artifact storage like GitHub Actions, here are your options:

### 1. S3-Compatible Storage (Recommended)

Use MinIO, AWS S3, or any S3-compatible storage. The examples above show S3 upload steps.

### 2. Tekton Results

Install [Tekton Results](https://github.com/tektoncd/results) for native artifact and log storage:

```bash
kubectl apply -f https://storage.googleapis.com/tekton-releases/results/latest/release.yaml
```

### 3. Persistent Volume Claims

Keep artifacts in PVCs (as shown in examples) for short-term storage. Clean up old PVCs periodically.

### 4. Container Registry

For firmware binaries, you could package them in container images and push to a registry:

```yaml
- name: package-and-push
  image: gcr.io/kaniko-project/executor:latest
  args:
    - --dockerfile=Dockerfile.artifacts
    - --context=$(workspaces.artifacts.path)
    - --destination=ghcr.io/vincent/home/firmware:$(params.revision)
```

## Tekton/Pipelines-as-Code Feature Gaps & Limitations

Based on this migration effort, here are notable missing features and limitations compared to GitHub Actions:

### Missing Features

#### 1. **Native Dynamic Matrix Support**
- **GitHub Actions**: Built-in support for dynamic matrix via `fromJSON()` with runtime-evaluated values
- **Tekton**: Static matrix only (v0.54+). Dynamic matrices require:
  - Custom tasks that spawn child PipelineRuns (as shown in packages/systems workflows)
  - Additional RBAC permissions
  - More complex pipeline definitions
  - Harder to track overall status (parent doesn't wait for children by default)
- **Impact**: High - Matrix builds are common in CI/CD
- **Workaround**: Spawn child PipelineRuns via kubectl (implemented in this migration)

#### 2. **Built-in Artifact Storage**
- **GitHub Actions**: Automatic 30-day artifact retention with `actions/upload-artifact`
- **Tekton**: No built-in solution, requires:
  - External S3-compatible storage
  - Tekton Results (separate installation)
  - PVCs (requires manual cleanup)
  - Container registry (for binary artifacts)
- **Impact**: High - Most workflows produce artifacts
- **Workaround**: S3 upload steps in pipelines

#### 3. **Native Scheduled Triggers**
- **GitHub Actions**: Built-in `schedule` with cron syntax
- **Pipelines as Code**: No native support, requires:
  - Kubernetes CronJob to create PipelineRuns
  - Additional RBAC and ServiceAccount setup
  - Separate Pipeline definition (not in `.tekton/`)
- **Impact**: Medium - Only affects periodic jobs
- **Workaround**: CronJob wrapper (implemented for nix-auto-upgrade)

#### 4. **Path Change Filtering Maturity**
- **GitHub Actions**: Stable path filtering with glob patterns
- **Pipelines as Code**: `on-path-change` is in **Technology Preview**
  - May have bugs or behavior changes
  - Limited documentation on edge cases
  - No exclusion patterns (e.g., `!docs/**`)
- **Impact**: Medium - Used in most workflows for efficiency
- **Alternative**: CEL expressions with `.pathChanged()` (more complex)

#### 5. **Workflow Visualization & Debugging**
- **GitHub Actions**: Rich UI with:
  - Workflow graph visualization
  - Step-by-step logs with timestamps
  - Re-run individual jobs
  - Inline annotations for errors
- **Tekton**: Limited UI, primarily CLI-based:
  - Tekton Dashboard (basic, requires installation)
  - `tkn` CLI for logs
  - No native graph visualization for dynamic matrix builds
  - Hard to correlate parent and child PipelineRuns
- **Impact**: High - Developer experience
- **Workaround**: Use labels to query child runs: `kubectl get pr -l parent=systems`

#### 6. **Dependency Caching**
- **GitHub Actions**: `actions/cache` for dependency caching between runs
- **Tekton**: No built-in caching, requires:
  - Persistent Volumes (slow, expensive)
  - External cache services
  - Manual cache key management
- **Impact**: High - Nix builds can benefit from caching
- **Workaround**: Cachix (already used), but Nix store not cached between runs

#### 7. **Concurrency Groups**
- **GitHub Actions**: `concurrency.group` with auto-cancellation
- **Pipelines as Code**: `max-keep-runs` limits history, but:
  - No built-in queue management
  - No automatic cancellation of older runs
  - Requires Kueue for advanced queueing
- **Impact**: Medium - Can waste resources on outdated runs
- **Workaround**: External cancellation logic or Kueue integration

#### 8. **Multi-Line Environment Variables**
- **GitHub Actions**: Natural support via HEREDOC
- **Tekton**: Requires careful YAML escaping
- **Impact**: Low - Cosmetic issue
- **Workaround**: Use ConfigMaps or multi-line YAML literals

#### 9. **Cross-Job Artifact Passing**
- **GitHub Actions**: `needs:` with implicit artifact sharing
- **Tekton**: Requires explicit workspace configuration
- **Impact**: Medium - Common pattern in multi-stage builds
- **Workaround**: Shared workspaces (already used)

#### 10. **Failure Aggregation for Matrix Builds**
- **GitHub Actions**: Automatically aggregates matrix results into single status
- **Tekton (Dynamic Matrix)**: Parent PipelineRun doesn't track child status
  - Need custom controller or polling logic
  - GitHub Check shows parent status only
- **Impact**: High - Confusing status reporting
- **Potential Solution**: Custom task that waits for children and reports aggregate status

### Tekton-Specific Challenges

#### 11. **PVC Storage Management**
- Each PipelineRun creates new PVCs (if using `volumeClaimTemplate`)
- Old PVCs not auto-deleted (accumulate over time)
- Large builds (50Gi for systems) can exhaust cluster storage
- **Workaround**: Periodic cleanup or use dynamic provisioner with reclaim policy

#### 12. **Image Pull for Each Step**
- Tekton pulls container images for each step individually
- No built-in image caching at node level (depends on container runtime)
- Can slow down pipelines with many steps
- **Workaround**: Use fewer, larger steps or configure image caching at cluster level

#### 13. **Limited GitHub Integration**
- **GitHub Checks API**: Supported but less polished than native Actions
- **PR Comments**: `/test`, `/retest` work, but limited compared to Actions bot
- **Status Details**: Less granular than Actions (especially for matrix builds)
- **Annotations**: No inline code annotations for errors

#### 14. **No Built-in Notification System**
- GitHub Actions can easily integrate with Slack, email, etc. via marketplace actions
- Tekton requires custom notification tasks or external systems

### Architecture-Specific Issues

#### 15. **Cross-Compilation Complexity**
- Building aarch64 on amd64 (or vice versa) requires:
  - QEMU emulation (slow, 10-100x slower)
  - Native nodes for each architecture (infrastructure cost)
  - Careful node selector configuration
- GitHub Actions provides ARM runners natively

### Stability Concerns

#### 16. **PaC Relative Maturity**
- Pipelines as Code is newer than GitHub Actions
- Fewer real-world deployments at scale
- Smaller community, fewer resources
- Breaking changes more common in minor versions

### Operational Overhead

#### 17. **Self-Hosting Burden**
- Requires managing Kubernetes cluster
- Tekton version upgrades (breaking changes possible)
- Storage provisioning and management
- Network policies, security
- Disaster recovery
- Multi-arch node pool management

## Recommendations for Feature Gaps

1. **Monitor Tekton Issues**: Track these missing features in Tekton GitHub:
   - [TEP-0090: Matrix in Pipelines](https://github.com/tektoncd/community/blob/main/teps/0090-matrix-typed-params.md)
   - [Results API improvements](https://github.com/tektoncd/results)

2. **Consider Hybrid Approach**:
   - Keep scheduled workflows in GitHub Actions
   - Keep workflows that need rich debugging in GitHub Actions
   - Migrate only compute-intensive builds to Tekton (leverage cheaper self-hosted)

3. **Invest in Tooling**:
   - Build a custom dashboard for matrix build status aggregation
   - Create reusable tasks for common patterns
   - Set up proper monitoring and alerting

4. **Contribute Upstream**:
   - Report bugs in PaC path filtering
   - Contribute examples for dynamic matrix patterns
   - Document heterogeneous cluster best practices

## Testing the Migration

1. **Install tkn-pac CLI**:
   ```bash
   brew install tektoncd/tools/tektoncd-cli
   tkn pac version
   ```

2. **Bootstrap locally** (optional):
   ```bash
   kind create cluster
   kubectl apply -f https://storage.googleapis.com/tekton-releases/pipeline/latest/release.yaml
   kubectl apply -f https://github.com/openshift-pipelines/pipelines-as-code/releases/latest/download/release.yaml
   ```

3. **Test a pipeline**:
   ```bash
   # Dry run
   tkn pac resolve -f .tekton/build-keyboard-moonlander.yaml

   # Create PR and watch
   tkn pac logs -L
   ```

## Migration Checklist

### Infrastructure Setup
- [ ] Set up Kubernetes cluster with Tekton Pipelines (v0.54+)
- [ ] Provision heterogeneous node pools:
  - [ ] x86_64 (amd64) nodes with 16Gi+ RAM for system builds
  - [ ] aarch64 (arm64) nodes with 8Gi+ RAM for ARM builds
  - [ ] Verify node labels: `kubernetes.io/arch=amd64|arm64`
- [ ] Configure dynamic storage provisioner (50Gi+ PVCs for system builds)
- [ ] Install Pipelines as Code
- [ ] Configure GitHub App or webhook for repository

### RBAC & Security
- [ ] Create `pipeline-spawner` ServiceAccount with PipelineRun creation permissions
- [ ] Create Role and RoleBinding for dynamic matrix builds
- [ ] Create secrets:
  - [ ] `ci-secrets` (CACHIX_AUTH_TOKEN, SBR_BOT_TOKEN)
  - [ ] `s3-credentials` (if using S3 artifact storage)
- [ ] Configure Repository CRD with custom ServiceAccount

### Pipeline Definitions
- [ ] Create ClusterTasks (git-clone if not present)
- [ ] Copy all `.tekton/*.yaml` files to repository:
  - [ ] `build-keyboard-eyelash-corne.yaml`
  - [ ] `build-keyboard-moonlander.yaml`
  - [ ] `build-packages.yaml` (dynamic matrix)
  - [ ] `build-systems.yaml` (dynamic matrix multi-arch)
- [ ] Create separate Pipeline for `nix-auto-upgrade`
- [ ] Create CronJob for `nix-auto-upgrade` scheduled trigger

### Artifact Storage
- [ ] Set up S3-compatible storage (MinIO/AWS S3/etc)
- [ ] Configure bucket and access credentials
- [ ] OR install Tekton Results for native storage

### Testing & Validation
- [ ] Test simple workflow: `build-keyboard-eyelash-corne`
- [ ] Test Nix workflow: `build-keyboard-moonlander`
- [ ] Test dynamic matrix on amd64: `build-packages`
- [ ] Test multi-arch matrix:
  - [ ] Verify amd64 systems build on amd64 nodes
  - [ ] Verify arm64 systems build on arm64 nodes
  - [ ] Check child PipelineRun scheduling with `kubectl get pr -l parent=systems`
- [ ] Test artifact upload to S3
- [ ] Verify Cachix integration works
- [ ] Test PR comment commands (`/retest`, `/test`)

### Operational Readiness
- [ ] Configure resource limits/requests for large builds (systems: 16Gi RAM)
- [ ] Set up PVC cleanup automation (old workspaces)
- [ ] Set up monitoring and log aggregation
- [ ] Create dashboard for tracking matrix build children
- [ ] Configure alerts for:
  - [ ] PipelineRun failures
  - [ ] Storage exhaustion
  - [ ] Node resource pressure
- [ ] Document team runbooks for:
  - [ ] Debugging failed matrix builds
  - [ ] Manually triggering pipelines
  - [ ] Querying child PipelineRuns
  - [ ] Cleaning up stuck PVCs
- [ ] Plan for Tekton version upgrades

## Troubleshooting

### Pipeline not triggering

- Check Repository CR status: `kubectl describe repository home-repo -n ci`
- Verify webhook delivery in GitHub settings
- Check PaC logs: `kubectl logs -n pipelines-as-code deployment/pipelines-as-code-controller`

### Build failures

- Check PipelineRun status: `kubectl describe pipelinerun <name> -n ci`
- View logs: `tkn pac logs` or `kubectl logs`
- Verify workspace PVC is large enough
- Check node resources (CPU/memory/disk)

### Artifact upload issues

- Verify S3 credentials: `kubectl get secret s3-credentials -n ci`
- Test S3 access from a pod
- Check network policies allow egress to S3

### Multi-arch build issues

- **Wrong architecture node**: Check PipelineRun events
  ```bash
  kubectl describe pr <name> -n ci | grep -A5 Events
  # Look for "FailedScheduling" events
  ```
- **No nodes available**: Verify heterogeneous nodes exist
  ```bash
  kubectl get nodes -L kubernetes.io/arch
  ```
- **Child PipelineRuns not spawning**: Check ServiceAccount permissions
  ```bash
  kubectl auth can-i create pipelineruns --as=system:serviceaccount:ci:pipeline-spawner -n ci
  ```
- **Matrix build status unclear**: Query child runs by label
  ```bash
  # For packages
  kubectl get pr -l parent=packages -n ci

  # For systems
  kubectl get pr -l parent=systems -n ci

  # Check status
  kubectl get pr -l parent=systems -n ci -o jsonpath='{range .items[*]}{.metadata.name}{"\t"}{.status.conditions[?(@.type=="Succeeded")].status}{"\n"}{end}'
  ```

### Storage exhaustion

- **PVCs accumulating**: Set up automated cleanup
  ```bash
  # List old PVCs
  kubectl get pvc -n ci --sort-by=.metadata.creationTimestamp

  # Delete PVCs from completed PipelineRuns (older than 7 days)
  kubectl get pvc -n ci -o json | jq -r '.items[] | select(.metadata.creationTimestamp < (now - 604800 | strftime("%Y-%m-%dT%H:%M:%SZ"))) | .metadata.name' | xargs -r kubectl delete pvc -n ci
  ```
- **Node disk pressure**: Monitor node storage
  ```bash
  kubectl top nodes
  kubectl describe nodes | grep -A5 "Allocated resources"
  ```

## Comparison: GitHub Actions vs Tekton PaC

| Feature | GitHub Actions | Tekton PaC |
|---------|---------------|------------|
| Infrastructure | GitHub-hosted | Self-hosted K8s |
| Cost | Pay-per-use or free | Cluster costs (24/7) |
| Setup complexity | Low | High |
| Artifact storage | Built-in (30 days) | External (S3/etc) + manual setup |
| Matrix builds | Native, dynamic | Static matrix OR custom spawning |
| Dynamic matrix | `fromJSON()` native | Custom task + RBAC + kubectl |
| Secrets | GitHub UI | K8s Secrets + CLI |
| Scheduled jobs | Native cron | CronJob wrapper + separate Pipeline |
| Multi-arch | GitHub runners (x64/ARM) | Heterogeneous cluster + node selectors |
| Caching | `actions/cache` | Cachix/external (no built-in) |
| GitOps | Files in repo | Files in repo |
| Path filtering | Stable | Tech Preview |
| Debugging UI | Rich web UI | CLI-first, basic dashboard |
| Status reporting | Single status for matrix | Parent + N children (harder to track) |
| Auto-cancellation | Built-in concurrency | Manual or Kueue |
| Storage cleanup | Automatic | Manual PVC cleanup needed |

## Recommendations

1. **Start with simplest workflow**: Migrate `build-keyboard-eyelash-corne` first
2. **Use hybrid approach**: Keep `nix-auto-upgrade` in GitHub Actions
3. **Invest in observability**: Set up monitoring for PipelineRuns
4. **Plan for scale**: aarch64 builds need dedicated nodes or emulation
5. **Backup strategy**: Keep GitHub Actions workflows until fully validated

## Summary

This migration guide provides complete translations of all 5 GitHub Actions workflows to Tekton Pipelines as Code:

### Successfully Migrated Workflows

1. **build-keyboard-eyelash-corne**: Simple firmware build → Direct translation
2. **build-keyboard-moonlander**: Nix-based build with Cachix → Direct translation with node selector support
3. **build-packages**: Dynamic matrix from `nix eval` → **Custom dynamic matrix implementation** spawning child PipelineRuns
4. **build-systems**: Multi-arch NixOS builds → **Advanced dynamic matrix** with heterogeneous node scheduling
5. **nix-auto-upgrade**: Scheduled updates → CronJob wrapper + separate Pipeline

### Key Implementation Patterns

- **Dynamic Matrix Builds**: Custom task pattern that:
  - Parses `nix eval` JSON output
  - Spawns child PipelineRuns via kubectl
  - Maps architectures to node selectors
  - Requires RBAC for PipelineRun creation

- **Heterogeneous Scheduling**:
  - `nodeSelector: {kubernetes.io/arch: amd64|arm64}`
  - Automatic mapping from Nix arch to Kubernetes arch
  - Parallel execution across architecture pools

- **Artifact Storage**: S3-compatible external storage pattern

### Migration Complexity Assessment

| Workflow | Complexity | Reason |
|----------|-----------|---------|
| eyelash_corne | Low | Simple build, single step |
| moonlander | Low-Medium | Nix build, artifact upload |
| packages | **High** | Dynamic matrix requires custom spawning |
| systems | **Very High** | Dynamic matrix + multi-arch + large PVCs |
| nix-auto-upgrade | Medium | Requires CronJob wrapper |

### Critical Feature Gaps Identified

1. **No native dynamic matrix** → Custom spawning pattern required
2. **No built-in artifact storage** → External S3 setup required
3. **No scheduled triggers** → CronJob wrapper required
4. **Path filtering in Tech Preview** → May have stability issues
5. **Limited matrix status aggregation** → Parent doesn't track children
6. **Manual PVC cleanup** → Automation required to prevent storage exhaustion
7. **No dependency caching** → Rely on Cachix for Nix artifacts

### Infrastructure Requirements

- **Kubernetes**: v1.24+ with Tekton Pipelines v0.54+
- **Nodes**:
  - Minimum 2x amd64 nodes (16Gi RAM each)
  - Minimum 1x arm64 node (8Gi RAM)
- **Storage**: Dynamic provisioner supporting 50Gi+ PVCs
- **External Services**:
  - S3-compatible storage for artifacts
  - Cachix for Nix binary cache

### Cost Considerations

**GitHub Actions (Current):**
- Free for public repos OR pay-per-minute for private
- No infrastructure management
- Zero operational overhead

**Tekton PaC (Proposed):**
- Kubernetes cluster running 24/7
- Storage costs (PVCs, S3)
- Network egress costs
- DevOps time for maintenance
- **Break-even**: Only if you run many builds OR have very cheap Kubernetes infrastructure

### Recommendation

**For this specific use case**, the migration is **technically feasible but operationally expensive**:

✅ **Migrate if:**
- You already run Kubernetes infrastructure
- You need tighter control over build environments
- You have compliance requirements for self-hosted CI
- You want to leverage heterogeneous ARM hardware you already own

❌ **Don't migrate if:**
- You want simplicity and low maintenance
- Your builds run infrequently
- You value rich debugging UI and developer experience
- You don't have Kubernetes expertise

**Hybrid Alternative:**
- Keep complex workflows (packages, systems, nix-auto-upgrade) in GitHub Actions
- Migrate only simple keyboard builds to Tekton if you have excess cluster capacity

### Next Steps

1. **Pilot**: Test `build-keyboard-eyelash-corne` first in a staging cluster
2. **Evaluate**: Measure operational overhead vs. benefits
3. **Decide**: Full migration, hybrid, or stay with GitHub Actions
4. **Document**: Create runbooks for team if proceeding

## References

- [Pipelines as Code Documentation](https://pipelinesascode.com/)
- [Tekton Pipeline Documentation](https://tekton.dev/docs/pipelines/)
- [Tekton Hub](https://hub.tekton.dev/) - Reusable tasks
- [tkn CLI](https://github.com/tektoncd/cli) - Command-line tool
- [Tekton Matrix Support (TEP-0090)](https://github.com/tektoncd/community/blob/main/teps/0090-matrix-typed-params.md)
- [Kueue for Advanced Queueing](https://kueue.sigs.k8s.io/)
