# Triggers Workflow

Tekton Triggers for event-driven pipeline execution via webhooks and automated CI/CD.

## When to Use

- Setting up GitHub/GitLab webhooks
- Automating pipeline execution from events
- Creating event listeners
- Configuring trigger bindings and templates
- Integrating with version control systems

## Trigger Components

### EventListener

**Listens for incoming HTTP events (webhooks)**

```yaml
apiVersion: triggers.tekton.dev/v1beta1
kind: EventListener
metadata:
  name: github-listener
spec:
  serviceAccountName: tekton-triggers-sa
  triggers:
    - name: github-push
      interceptors:
        - ref:
            name: "github"
          params:
            - name: "secretRef"
              value:
                secretName: github-secret
                secretKey: secretToken
            - name: "eventTypes"
              value: ["push"]
      bindings:
        - ref: github-push-binding
      template:
        ref: pipeline-template
```

### TriggerBinding

**Extracts data from event payload**

```yaml
apiVersion: triggers.tekton.dev/v1beta1
kind: TriggerBinding
metadata:
  name: github-push-binding
spec:
  params:
    - name: gitrevision
      value: $(body.head_commit.id)
    - name: gitrepositoryurl
      value: $(body.repository.clone_url)
    - name: gitbranch
      value: $(body.ref)
    - name: gitauthor
      value: $(body.head_commit.author.username)
    - name: gitmessage
      value: $(body.head_commit.message)
```

### TriggerTemplate

**Creates PipelineRun or TaskRun from event**

```yaml
apiVersion: triggers.tekton.dev/v1beta1
kind: TriggerTemplate
metadata:
  name: pipeline-template
spec:
  params:
    - name: gitrevision
      description: Git commit SHA
    - name: gitrepositoryurl
      description: Repository URL
    - name: gitbranch
      description: Git branch ref
  resourcetemplates:
    - apiVersion: tekton.dev/v1
      kind: PipelineRun
      metadata:
        generateName: ci-pipeline-run-
      spec:
        pipelineRef:
          name: ci-pipeline
        params:
          - name: git-url
            value: $(tt.params.gitrepositoryurl)
          - name: git-revision
            value: $(tt.params.gitrevision)
        workspaces:
          - name: source-code
            volumeClaimTemplate:
              spec:
                accessModes:
                  - ReadWriteOnce
                resources:
                  requests:
                    storage: 1Gi
```

## GitHub Integration

### Complete GitHub Webhook Setup

**1. Create Secret for Webhook Validation**

```yaml
apiVersion: v1
kind: Secret
metadata:
  name: github-secret
type: Opaque
stringData:
  secretToken: "your-webhook-secret-here"
```

**2. Create ServiceAccount with RBAC**

```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: tekton-triggers-sa
---
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: tekton-triggers-role
rules:
  - apiGroups: ["triggers.tekton.dev"]
    resources: ["eventlisteners", "triggerbindings", "triggertemplates"]
    verbs: ["get"]
  - apiGroups: ["tekton.dev"]
    resources: ["pipelineruns", "pipelineresources", "taskruns"]
    verbs: ["create"]
---
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: tekton-triggers-binding
subjects:
  - kind: ServiceAccount
    name: tekton-triggers-sa
roleRef:
  apiGroup: rbac.authorization.k8s.io
  kind: Role
  name: tekton-triggers-role
```

**3. Create EventListener**

```yaml
apiVersion: triggers.tekton.dev/v1beta1
kind: EventListener
metadata:
  name: github-listener
spec:
  serviceAccountName: tekton-triggers-sa
  triggers:
    - name: github-push
      interceptors:
        - ref:
            name: "github"
          params:
            - name: "secretRef"
              value:
                secretName: github-secret
                secretKey: secretToken
            - name: "eventTypes"
              value: ["push", "pull_request"]
      bindings:
        - ref: github-binding
      template:
        ref: pipeline-template
```

**4. Expose EventListener**

```bash
# Get event listener service
kubectl get svc el-github-listener

# Expose via Ingress
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: github-webhook
  annotations:
    cert-manager.io/cluster-issuer: letsencrypt-prod
spec:
  ingressClassName: nginx
  tls:
    - hosts:
        - webhooks.example.com
      secretName: webhook-tls
  rules:
    - host: webhooks.example.com
      http:
        paths:
          - path: /
            pathType: Prefix
            backend:
              service:
                name: el-github-listener
                port:
                  number: 8080
```

**5. Configure GitHub Webhook**

- Go to GitHub repository Settings → Webhooks
- Payload URL: `https://webhooks.example.com`
- Content type: `application/json`
- Secret: Same value as in `github-secret`
- Events: Push, Pull Request
- Active: ✓

### GitHub TriggerBinding Examples

**Push event:**
```yaml
spec:
  params:
    - name: gitrevision
      value: $(body.head_commit.id)
    - name: gitrepositoryurl
      value: $(body.repository.clone_url)
    - name: gitbranch
      value: $(extensions.branch_name)
```

**Pull Request event:**
```yaml
spec:
  params:
    - name: pr-number
      value: $(body.pull_request.number)
    - name: pr-title
      value: $(body.pull_request.title)
    - name: pr-head-sha
      value: $(body.pull_request.head.sha)
    - name: pr-base-branch
      value: $(body.pull_request.base.ref)
```

## GitLab Integration

### GitLab EventListener

```yaml
apiVersion: triggers.tekton.dev/v1beta1
kind: EventListener
metadata:
  name: gitlab-listener
spec:
  serviceAccountName: tekton-triggers-sa
  triggers:
    - name: gitlab-push
      interceptors:
        - ref:
            name: "gitlab"
          params:
            - name: "secretRef"
              value:
                secretName: gitlab-secret
                secretKey: secretToken
            - name: "eventTypes"
              value: ["Push Hook", "Merge Request Hook"]
      bindings:
        - ref: gitlab-binding
      template:
        ref: pipeline-template
```

### GitLab TriggerBinding

```yaml
apiVersion: triggers.tekton.dev/v1beta1
kind: TriggerBinding
metadata:
  name: gitlab-binding
spec:
  params:
    - name: gitrevision
      value: $(body.checkout_sha)
    - name: gitrepositoryurl
      value: $(body.project.git_http_url)
    - name: gitbranch
      value: $(body.ref)
    - name: project-name
      value: $(body.project.name)
```

## Interceptors

### CEL Interceptor (Custom Logic)

```yaml
interceptors:
  - name: "filter-branch"
    ref:
      name: "cel"
    params:
      - name: "filter"
        value: "body.ref.startsWith('refs/heads/main')"
      - name: "overlays"
        value:
          - key: branch_name
            expression: "body.ref.split('/')[2]"
```

### Webhook Interceptor (Custom Service)

```yaml
interceptors:
  - name: "custom-logic"
    webhook:
      objectRef:
        apiVersion: v1
        kind: Service
        name: custom-interceptor
        namespace: default
      header:
        - name: X-Custom-Header
          value: "custom-value"
```

## Advanced Patterns

### Multiple Triggers in One EventListener

```yaml
spec:
  triggers:
    - name: main-branch
      interceptors:
        - ref:
            name: "cel"
          params:
            - name: "filter"
              value: "body.ref == 'refs/heads/main'"
      bindings:
        - ref: prod-binding
      template:
        ref: prod-template

    - name: feature-branch
      interceptors:
        - ref:
            name: "cel"
          params:
            - name: "filter"
              value: "body.ref.startsWith('refs/heads/feature/')"
      bindings:
        - ref: dev-binding
      template:
        ref: dev-template
```

### Conditional Pipeline Execution

```yaml
# In TriggerTemplate
spec:
  resourcetemplates:
    - apiVersion: tekton.dev/v1
      kind: PipelineRun
      metadata:
        generateName: conditional-run-
      spec:
        pipelineRef:
          name: my-pipeline
        params:
          - name: deploy-to-prod
            value: $(tt.params.gitbranch == "refs/heads/main")
```

### ClusterTriggerBinding (Cluster-Wide Reuse)

```yaml
apiVersion: triggers.tekton.dev/v1beta1
kind: ClusterTriggerBinding
metadata:
  name: common-git-binding
spec:
  params:
    - name: gitrevision
      value: $(body.head_commit.id)
    - name: gitrepositoryurl
      value: $(body.repository.clone_url)
```

## Testing Triggers

### Test with curl

```bash
# Get EventListener URL
EL_URL=$(kubectl get eventlistener github-listener -o jsonpath='{.status.address.url}')

# Send test payload
curl -X POST $EL_URL \
  -H "Content-Type: application/json" \
  -H "X-GitHub-Event: push" \
  -H "X-Hub-Signature: sha1=..." \
  -d @test-payload.json

# Watch for pipelinerun creation
kubectl get pipelineruns -w
```

### Test Payload Examples

**GitHub push:**
```json
{
  "ref": "refs/heads/main",
  "head_commit": {
    "id": "abc123...",
    "message": "Test commit",
    "author": {
      "username": "testuser"
    }
  },
  "repository": {
    "clone_url": "https://github.com/org/repo.git",
    "name": "repo"
  }
}
```

## Troubleshooting

### EventListener Not Receiving Events

```bash
# Check EventListener pod
kubectl get pods -l eventlistener=github-listener

# Check logs
kubectl logs -l eventlistener=github-listener

# Check service
kubectl get svc el-github-listener

# Test connectivity
kubectl port-forward svc/el-github-listener 8080:8080
curl -X POST http://localhost:8080 -d '{}'
```

### Webhook Validation Failing

```bash
# Check secret exists
kubectl get secret github-secret

# Verify secret value matches GitHub
kubectl get secret github-secret -o jsonpath='{.data.secretToken}' | base64 -d

# Check interceptor logs
kubectl logs -l eventlistener=github-listener | grep "validation failed"
```

### PipelineRun Not Created

```bash
# Check EventListener logs
kubectl logs -l eventlistener=github-listener

# Check RBAC permissions
kubectl auth can-i create pipelineruns --as=system:serviceaccount:default:tekton-triggers-sa

# Check TriggerTemplate
kubectl get triggertemplate pipeline-template -o yaml
```

### Wrong Parameters Extracted

```bash
# View received payload
kubectl logs -l eventlistener=github-listener | grep "event body"

# Test TriggerBinding
# Create test EventListener with logging
# Examine extracted parameters in logs
```

## Best Practices

### Security
- Always validate webhooks with secrets
- Use HTTPS for webhook endpoints
- Limit webhook events to necessary types
- Use dedicated ServiceAccount with minimal permissions
- Never expose raw event data in logs

### Reliability
- Set resource limits on EventListener pods
- Use horizontal pod autoscaling for high traffic
- Implement retry logic in pipelines
- Monitor EventListener availability
- Use Ingress with TLS termination

### Maintainability
- Use ClusterTriggerBinding for common parameters
- Keep TriggerBindings focused and reusable
- Document expected event payload structure
- Version your TriggerTemplates
- Use CEL interceptors for complex filtering

### Performance
- Filter events early with interceptors
- Avoid creating unnecessary PipelineRuns
- Use volumeClaimTemplate for workspace isolation
- Set appropriate timeouts
- Clean up old PipelineRuns with pruner

## Common Patterns

### Multi-Repository Trigger

```yaml
spec:
  triggers:
    - name: repo-a
      interceptors:
        - ref:
            name: "cel"
          params:
            - name: "filter"
              value: "body.repository.name == 'repo-a'"
      template:
        ref: repo-a-template

    - name: repo-b
      interceptors:
        - ref:
            name: "cel"
          params:
            - name: "filter"
              value: "body.repository.name == 'repo-b'"
      template:
        ref: repo-b-template
```

### Branch-Based Pipeline Selection

```yaml
spec:
  triggers:
    - name: main-branch
      interceptors:
        - ref:
            name: "cel"
          params:
            - name: "filter"
              value: "body.ref == 'refs/heads/main'"
      template:
        ref: production-pipeline-template

    - name: develop-branch
      interceptors:
        - ref:
            name: "cel"
          params:
            - name: "filter"
              value: "body.ref == 'refs/heads/develop'"
      template:
        ref: staging-pipeline-template
```
