# Deploy Workflow

Best practices for deploying applications to Kubernetes with proper validation and rollout management.

## When to Use

- Deploying new applications to cluster
- Updating existing deployments
- Rolling out configuration changes
- Managing deployment strategies

## Deployment Best Practices

### Pre-Deployment Checklist

- [ ] Manifests validated with `kubectl apply --dry-run=client`
- [ ] Resource limits and requests defined
- [ ] Health probes configured (readiness, liveness)
- [ ] Security context set (non-root user)
- [ ] Labels and selectors properly configured
- [ ] Secrets and ConfigMaps created
- [ ] Image tags are specific (not `:latest`)
- [ ] Namespace exists
- [ ] RBAC permissions configured
- [ ] Pod disruption budget defined (for production)

## Deployment Steps

### 1. Validate Manifests

```bash
# Dry-run validation (client-side)
kubectl apply --dry-run=client -f deployment.yaml

# Dry-run validation (server-side, with validation)
kubectl apply --dry-run=server -f deployment.yaml

# Validate with kubectl diff
kubectl diff -f deployment.yaml

# Validate YAML syntax
kubectl apply --dry-run=client -f deployment.yaml -o yaml
```

### 2. Create Namespace (if needed)

```bash
# Create namespace
kubectl create namespace myapp

# Or use manifest
cat <<EOF | kubectl apply -f -
apiVersion: v1
kind: Namespace
metadata:
  name: myapp
  labels:
    name: myapp
EOF
```

### 3. Create ConfigMaps and Secrets

```bash
# Create ConfigMap from file
kubectl create configmap myapp-config --from-file=config.json

# Create ConfigMap from literal
kubectl create configmap myapp-config \
  --from-literal=database_url=postgres://db:5432/myapp \
  --from-literal=cache_ttl=3600

# Create Secret from file
kubectl create secret generic myapp-secret --from-file=credentials.json

# Create Secret from literal
kubectl create secret generic myapp-secret \
  --from-literal=api_key=supersecret123

# Create TLS secret
kubectl create secret tls myapp-tls \
  --cert=path/to/cert.pem \
  --key=path/to/key.pem
```

### 4. Apply Deployment

```bash
# Apply single file
kubectl apply -f deployment.yaml

# Apply directory
kubectl apply -f ./k8s/

# Apply with specific namespace
kubectl apply -f deployment.yaml -n myapp

# Apply and record the command (for rollback)
kubectl apply -f deployment.yaml --record

# Apply with validation
kubectl apply -f deployment.yaml --validate=true
```

### 5. Monitor Rollout

```bash
# Check rollout status
kubectl rollout status deployment/myapp

# Watch deployment progress
kubectl get deployment myapp -w

# Check pods during rollout
kubectl get pods -l app=myapp -w

# Check events
kubectl get events -w
```

### 6. Verify Deployment

```bash
# Get deployment status
kubectl get deployment myapp

# Check pods are running
kubectl get pods -l app=myapp

# Check replica set
kubectl get rs -l app=myapp

# Verify service endpoints
kubectl get endpoints myapp

# Test service connectivity
kubectl port-forward service/myapp 8080:80
curl http://localhost:8080/healthz
```

## Deployment Strategies

### Rolling Update (Default)

**Best for:** Most applications, zero-downtime deployments

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: myapp
spec:
  replicas: 3
  strategy:
    type: RollingUpdate
    rollingUpdate:
      maxSurge: 1        # Max pods above desired count
      maxUnavailable: 0  # Max pods unavailable during update
  template:
    spec:
      containers:
      - name: myapp
        image: myapp:v2.0.0
```

**Commands:**
```bash
# Update image
kubectl set image deployment/myapp myapp=myapp:v2.0.0

# Monitor rollout
kubectl rollout status deployment/myapp

# Pause rollout
kubectl rollout pause deployment/myapp

# Resume rollout
kubectl rollout resume deployment/myapp
```

### Recreate Strategy

**Best for:** Incompatible version changes, dev environments

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: myapp
spec:
  replicas: 3
  strategy:
    type: Recreate
  template:
    spec:
      containers:
      - name: myapp
        image: myapp:v2.0.0
```

### Blue/Green Deployment

**Best for:** Zero-downtime with instant rollback capability

```bash
# Deploy green version alongside blue
kubectl apply -f deployment-green.yaml

# Verify green is healthy
kubectl get pods -l version=green

# Switch traffic by updating service selector
kubectl patch service myapp -p '{"spec":{"selector":{"version":"green"}}}'

# Monitor and rollback if needed
kubectl patch service myapp -p '{"spec":{"selector":{"version":"blue"}}}'

# Clean up old version
kubectl delete deployment myapp-blue
```

### Canary Deployment

**Best for:** Gradual rollout with risk mitigation

```bash
# Deploy canary with fewer replicas
kubectl apply -f deployment-canary.yaml

# Monitor metrics and logs
kubectl logs -l version=canary -f

# Gradually increase canary replicas
kubectl scale deployment myapp-canary --replicas=2

# If successful, update main deployment
kubectl apply -f deployment-main.yaml

# Clean up canary
kubectl delete deployment myapp-canary
```

## Rollback Procedures

### Quick Rollback

```bash
# Rollback to previous version
kubectl rollout undo deployment/myapp

# Rollback to specific revision
kubectl rollout undo deployment/myapp --to-revision=2

# Check rollback status
kubectl rollout status deployment/myapp
```

### View Rollout History

```bash
# View deployment history
kubectl rollout history deployment/myapp

# View specific revision
kubectl rollout history deployment/myapp --revision=3

# Compare revisions
kubectl rollout history deployment/myapp --revision=2 > rev2.yaml
kubectl rollout history deployment/myapp --revision=3 > rev3.yaml
diff rev2.yaml rev3.yaml
```

## Production Deployment Template

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: myapp
  namespace: production
  labels:
    app.kubernetes.io/name: myapp
    app.kubernetes.io/version: "2.0.0"
    app.kubernetes.io/component: backend
spec:
  replicas: 3
  revisionHistoryLimit: 10
  strategy:
    type: RollingUpdate
    rollingUpdate:
      maxSurge: 1
      maxUnavailable: 0
  selector:
    matchLabels:
      app.kubernetes.io/name: myapp
  template:
    metadata:
      labels:
        app.kubernetes.io/name: myapp
        app.kubernetes.io/version: "2.0.0"
        app.kubernetes.io/component: backend
      annotations:
        prometheus.io/scrape: "true"
        prometheus.io/port: "8080"
        prometheus.io/path: "/metrics"
    spec:
      serviceAccountName: myapp
      securityContext:
        runAsNonRoot: true
        runAsUser: 1000
        fsGroup: 1000
        seccompProfile:
          type: RuntimeDefault
      containers:
      - name: myapp
        image: myregistry.com/myapp:2.0.0
        imagePullPolicy: IfNotPresent
        ports:
        - name: http
          containerPort: 8080
          protocol: TCP
        env:
        - name: POD_NAME
          valueFrom:
            fieldRef:
              fieldPath: metadata.name
        - name: POD_NAMESPACE
          valueFrom:
            fieldRef:
              fieldPath: metadata.namespace
        envFrom:
        - configMapRef:
            name: myapp-config
        - secretRef:
            name: myapp-secret
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
            port: http
          initialDelaySeconds: 30
          periodSeconds: 10
          timeoutSeconds: 5
          failureThreshold: 3
        readinessProbe:
          httpGet:
            path: /ready
            port: http
          initialDelaySeconds: 5
          periodSeconds: 5
          timeoutSeconds: 3
          failureThreshold: 3
        startupProbe:
          httpGet:
            path: /healthz
            port: http
          initialDelaySeconds: 0
          periodSeconds: 5
          timeoutSeconds: 3
          failureThreshold: 30
        securityContext:
          allowPrivilegeEscalation: false
          readOnlyRootFilesystem: true
          runAsNonRoot: true
          runAsUser: 1000
          capabilities:
            drop:
            - ALL
        volumeMounts:
        - name: tmp
          mountPath: /tmp
        - name: cache
          mountPath: /app/cache
      volumes:
      - name: tmp
        emptyDir: {}
      - name: cache
        emptyDir: {}
      affinity:
        podAntiAffinity:
          preferredDuringSchedulingIgnoredDuringExecution:
          - weight: 100
            podAffinityTerm:
              labelSelector:
                matchLabels:
                  app.kubernetes.io/name: myapp
              topologyKey: kubernetes.io/hostname
---
apiVersion: v1
kind: Service
metadata:
  name: myapp
  namespace: production
  labels:
    app.kubernetes.io/name: myapp
spec:
  type: ClusterIP
  selector:
    app.kubernetes.io/name: myapp
  ports:
  - name: http
    port: 80
    targetPort: http
    protocol: TCP
---
apiVersion: policy/v1
kind: PodDisruptionBudget
metadata:
  name: myapp
  namespace: production
spec:
  minAvailable: 2
  selector:
    matchLabels:
      app.kubernetes.io/name: myapp
```

## Update Strategies

### Update Image Only

```bash
# Update image for specific container
kubectl set image deployment/myapp myapp=myapp:v2.0.0

# Update with recording for rollback
kubectl set image deployment/myapp myapp=myapp:v2.0.0 --record
```

### Update Environment Variables

```bash
# Update ConfigMap
kubectl create configmap myapp-config --from-literal=new_key=new_value --dry-run=client -o yaml | kubectl apply -f -

# Restart deployment to pick up changes
kubectl rollout restart deployment/myapp
```

### Patch Deployment

```bash
# Patch replicas
kubectl patch deployment myapp -p '{"spec":{"replicas":5}}'

# Patch strategy
kubectl patch deployment myapp -p '{"spec":{"strategy":{"type":"Recreate"}}}'

# Patch with JSON file
kubectl patch deployment myapp --patch-file=patch.json
```

## GitOps Best Practices

### Recommended Workflow

1. Store manifests in Git repository
2. Use Kustomize or Helm for templating
3. CI/CD pipeline validates manifests
4. Automated deployment via GitOps tool (ArgoCD, Flux)
5. Manual approval for production
6. Automated rollback on failures

### Kustomize Example

```bash
# Directory structure
# base/
#   deployment.yaml
#   service.yaml
#   kustomization.yaml
# overlays/
#   dev/
#     kustomization.yaml
#   prod/
#     kustomization.yaml

# Apply with kustomize
kubectl apply -k overlays/prod/

# Preview changes
kubectl diff -k overlays/prod/
```

## Troubleshooting Deployments

### Deployment Not Progressing

```bash
# Check deployment status
kubectl describe deployment myapp

# Look for conditions
kubectl get deployment myapp -o jsonpath='{.status.conditions}'

# Check replica set
kubectl get rs -l app=myapp
kubectl describe rs <replicaset-name>
```

### Pods Not Starting

```bash
# Check pod status
kubectl get pods -l app=myapp

# Describe problematic pod
kubectl describe pod <pod-name>

# Check events
kubectl get events --sort-by=.metadata.creationTimestamp
```

### Resource Quota Issues

```bash
# Check resource quotas
kubectl get resourcequota

# Describe quota
kubectl describe resourcequota <quota-name>

# Check current usage
kubectl top pods
```

## Post-Deployment Tasks

```bash
# Verify deployment health
kubectl get deployment myapp
kubectl get pods -l app=myapp

# Check logs
kubectl logs -l app=myapp --tail=100

# Test endpoint
kubectl port-forward service/myapp 8080:80
curl http://localhost:8080/healthz

# Monitor metrics (if metrics-server installed)
kubectl top pods -l app=myapp

# Clean up old replica sets
kubectl delete rs <old-replicaset-name>
```

## Production Deployment Checklist

- [ ] Manifests stored in version control
- [ ] Dry-run validation passed
- [ ] Security scan completed (image vulnerabilities)
- [ ] Resource limits appropriate for load
- [ ] Health probes tested and validated
- [ ] Pod disruption budget configured
- [ ] Monitoring and alerting configured
- [ ] Rollback procedure documented
- [ ] Deployment tested in staging
- [ ] Change approval obtained
- [ ] Deployment window scheduled
- [ ] Stakeholders notified
