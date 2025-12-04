# Resources Workflow

Creating and managing Kubernetes resources including manifests, ConfigMaps, Secrets, and other workload types.

## When to Use

- Creating new Kubernetes resources
- Writing or modifying YAML manifests
- Managing ConfigMaps and Secrets
- Working with different workload types

## Resource Creation Methods

### Imperative vs Declarative

**Imperative (quick testing, not recommended for production):**
```bash
kubectl create deployment nginx --image=nginx
kubectl expose deployment nginx --port=80
kubectl scale deployment nginx --replicas=3
```

**Declarative (recommended for production):**
```bash
kubectl apply -f deployment.yaml
kubectl apply -f service.yaml
kubectl apply -k ./manifests/
```

## Common Resource Types

### Deployment

**Use for:** Stateless applications, web servers, APIs

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: myapp
  namespace: production
  labels:
    app.kubernetes.io/name: myapp
    app.kubernetes.io/version: "1.0.0"
spec:
  replicas: 3
  selector:
    matchLabels:
      app.kubernetes.io/name: myapp
  template:
    metadata:
      labels:
        app.kubernetes.io/name: myapp
        app.kubernetes.io/version: "1.0.0"
    spec:
      containers:
      - name: myapp
        image: myapp:1.0.0
        ports:
        - containerPort: 8080
          name: http
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
        readinessProbe:
          httpGet:
            path: /ready
            port: http
          initialDelaySeconds: 5
          periodSeconds: 5
```

### StatefulSet

**Use for:** Databases, stateful applications requiring stable network identity and persistent storage

```yaml
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: postgres
  namespace: production
spec:
  serviceName: postgres
  replicas: 3
  selector:
    matchLabels:
      app: postgres
  template:
    metadata:
      labels:
        app: postgres
    spec:
      containers:
      - name: postgres
        image: postgres:15
        ports:
        - containerPort: 5432
          name: postgres
        env:
        - name: POSTGRES_PASSWORD
          valueFrom:
            secretKeyRef:
              name: postgres-secret
              key: password
        - name: PGDATA
          value: /var/lib/postgresql/data/pgdata
        volumeMounts:
        - name: data
          mountPath: /var/lib/postgresql/data
        resources:
          requests:
            cpu: 500m
            memory: 1Gi
          limits:
            cpu: 2000m
            memory: 4Gi
  volumeClaimTemplates:
  - metadata:
      name: data
    spec:
      accessModes: ["ReadWriteOnce"]
      storageClassName: fast-ssd
      resources:
        requests:
          storage: 100Gi
---
apiVersion: v1
kind: Service
metadata:
  name: postgres
  namespace: production
spec:
  clusterIP: None  # Headless service for StatefulSet
  selector:
    app: postgres
  ports:
  - port: 5432
    targetPort: postgres
    name: postgres
```

### DaemonSet

**Use for:** Node-level services (logging, monitoring, network plugins)

```yaml
apiVersion: apps/v1
kind: DaemonSet
metadata:
  name: node-exporter
  namespace: monitoring
spec:
  selector:
    matchLabels:
      app: node-exporter
  template:
    metadata:
      labels:
        app: node-exporter
    spec:
      hostNetwork: true
      hostPID: true
      containers:
      - name: node-exporter
        image: prom/node-exporter:latest
        args:
        - --path.procfs=/host/proc
        - --path.sysfs=/host/sys
        - --collector.filesystem.mount-points-exclude=^/(sys|proc|dev|host|etc)($$|/)
        ports:
        - containerPort: 9100
          protocol: TCP
          name: metrics
        resources:
          requests:
            cpu: 100m
            memory: 128Mi
          limits:
            cpu: 200m
            memory: 256Mi
        volumeMounts:
        - name: proc
          mountPath: /host/proc
          readOnly: true
        - name: sys
          mountPath: /host/sys
          readOnly: true
      volumes:
      - name: proc
        hostPath:
          path: /proc
      - name: sys
        hostPath:
          path: /sys
      tolerations:
      - effect: NoSchedule
        operator: Exists
```

### Job

**Use for:** One-time batch jobs, database migrations, backups

```yaml
apiVersion: batch/v1
kind: Job
metadata:
  name: database-migration
  namespace: production
spec:
  ttlSecondsAfterFinished: 86400  # Clean up after 24 hours
  backoffLimit: 3
  template:
    metadata:
      labels:
        app: migration
    spec:
      restartPolicy: OnFailure
      containers:
      - name: migrate
        image: myapp:1.0.0
        command: ["/app/migrate"]
        args: ["--direction", "up"]
        env:
        - name: DATABASE_URL
          valueFrom:
            secretKeyRef:
              name: db-secret
              key: url
        resources:
          requests:
            cpu: 100m
            memory: 128Mi
          limits:
            cpu: 500m
            memory: 512Mi
```

### CronJob

**Use for:** Scheduled tasks, periodic backups, cleanup jobs

```yaml
apiVersion: batch/v1
kind: CronJob
metadata:
  name: backup-job
  namespace: production
spec:
  schedule: "0 2 * * *"  # Daily at 2 AM
  timeZone: "America/New_York"
  concurrencyPolicy: Forbid
  successfulJobsHistoryLimit: 3
  failedJobsHistoryLimit: 3
  jobTemplate:
    spec:
      ttlSecondsAfterFinished: 86400
      template:
        metadata:
          labels:
            app: backup
        spec:
          restartPolicy: OnFailure
          containers:
          - name: backup
            image: backup-tool:latest
            command: ["/backup.sh"]
            env:
            - name: BACKUP_TARGET
              value: s3://my-backups/
            - name: AWS_ACCESS_KEY_ID
              valueFrom:
                secretKeyRef:
                  name: aws-credentials
                  key: access-key-id
            - name: AWS_SECRET_ACCESS_KEY
              valueFrom:
                secretKeyRef:
                  name: aws-credentials
                  key: secret-access-key
            resources:
              requests:
                cpu: 100m
                memory: 256Mi
              limits:
                cpu: 500m
                memory: 1Gi
```

## ConfigMaps and Secrets

### ConfigMap

**Use for:** Non-sensitive configuration data

**From literals:**
```bash
kubectl create configmap myapp-config \
  --from-literal=database_host=postgres.production.svc.cluster.local \
  --from-literal=database_port=5432 \
  --from-literal=log_level=info
```

**From file:**
```bash
kubectl create configmap myapp-config --from-file=config.json
kubectl create configmap nginx-config --from-file=nginx.conf
```

**From directory:**
```bash
kubectl create configmap app-configs --from-file=./configs/
```

**Declarative YAML:**
```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: myapp-config
  namespace: production
data:
  database_host: postgres.production.svc.cluster.local
  database_port: "5432"
  log_level: info
  config.json: |
    {
      "feature_flags": {
        "new_ui": true,
        "beta_features": false
      },
      "cache_ttl": 3600
    }
```

**Using ConfigMap in Pod:**
```yaml
apiVersion: v1
kind: Pod
metadata:
  name: myapp
spec:
  containers:
  - name: myapp
    image: myapp:1.0.0
    # Option 1: All keys as environment variables
    envFrom:
    - configMapRef:
        name: myapp-config
    # Option 2: Specific keys as environment variables
    env:
    - name: DATABASE_HOST
      valueFrom:
        configMapKeyRef:
          name: myapp-config
          key: database_host
    # Option 3: Mount as volume
    volumeMounts:
    - name: config
      mountPath: /etc/config
  volumes:
  - name: config
    configMap:
      name: myapp-config
```

### Secret

**Use for:** Sensitive data (passwords, tokens, certificates)

**From literals:**
```bash
kubectl create secret generic myapp-secret \
  --from-literal=api_key=supersecret123 \
  --from-literal=database_password=pass123
```

**From file:**
```bash
kubectl create secret generic myapp-secret --from-file=credentials.json
kubectl create secret generic ssh-key --from-file=ssh-privatekey=~/.ssh/id_rsa
```

**TLS secret:**
```bash
kubectl create secret tls myapp-tls \
  --cert=path/to/cert.pem \
  --key=path/to/key.pem
```

**Docker registry secret:**
```bash
kubectl create secret docker-registry regcred \
  --docker-server=myregistry.com \
  --docker-username=user \
  --docker-password=pass \
  --docker-email=user@example.com
```

**Declarative YAML:**
```yaml
apiVersion: v1
kind: Secret
metadata:
  name: myapp-secret
  namespace: production
type: Opaque
stringData:  # Use stringData for plain text (will be base64 encoded)
  api_key: supersecret123
  database_password: pass123
# Or use data for base64 encoded values
data:
  api_key: c3VwZXJzZWNyZXQxMjM=
```

**Using Secret in Pod:**
```yaml
apiVersion: v1
kind: Pod
metadata:
  name: myapp
spec:
  containers:
  - name: myapp
    image: myapp:1.0.0
    # Option 1: All keys as environment variables
    envFrom:
    - secretRef:
        name: myapp-secret
    # Option 2: Specific keys as environment variables
    env:
    - name: API_KEY
      valueFrom:
        secretKeyRef:
          name: myapp-secret
          key: api_key
    # Option 3: Mount as volume (RECOMMENDED for sensitive data)
    volumeMounts:
    - name: secrets
      mountPath: /etc/secrets
      readOnly: true
  # Use image pull secret
  imagePullSecrets:
  - name: regcred
  volumes:
  - name: secrets
    secret:
      secretName: myapp-secret
      defaultMode: 0400  # Read-only for owner
```

## Service Types

### ClusterIP (Default)

**Use for:** Internal service-to-service communication

```yaml
apiVersion: v1
kind: Service
metadata:
  name: backend
  namespace: production
spec:
  type: ClusterIP
  selector:
    app: backend
  ports:
  - name: http
    port: 80
    targetPort: 8080
    protocol: TCP
```

### NodePort

**Use for:** Exposing service on each node's IP at a static port

```yaml
apiVersion: v1
kind: Service
metadata:
  name: frontend
  namespace: production
spec:
  type: NodePort
  selector:
    app: frontend
  ports:
  - name: http
    port: 80
    targetPort: 8080
    nodePort: 30080  # Optional: 30000-32767 range
    protocol: TCP
```

### LoadBalancer

**Use for:** Exposing service externally using cloud provider's load balancer

```yaml
apiVersion: v1
kind: Service
metadata:
  name: web
  namespace: production
  annotations:
    service.beta.kubernetes.io/aws-load-balancer-type: nlb
spec:
  type: LoadBalancer
  selector:
    app: web
  ports:
  - name: http
    port: 80
    targetPort: 8080
  - name: https
    port: 443
    targetPort: 8443
```

### Headless Service

**Use for:** StatefulSet, direct pod-to-pod communication

```yaml
apiVersion: v1
kind: Service
metadata:
  name: cassandra
  namespace: production
spec:
  clusterIP: None  # Headless
  selector:
    app: cassandra
  ports:
  - port: 9042
    name: cql
```

## Ingress

**Use for:** HTTP/HTTPS routing to services

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: myapp-ingress
  namespace: production
  annotations:
    cert-manager.io/cluster-issuer: letsencrypt-prod
    nginx.ingress.kubernetes.io/ssl-redirect: "true"
    nginx.ingress.kubernetes.io/rate-limit: "100"
spec:
  ingressClassName: nginx
  tls:
  - hosts:
    - myapp.example.com
    secretName: myapp-tls
  rules:
  - host: myapp.example.com
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: frontend
            port:
              number: 80
      - path: /api
        pathType: Prefix
        backend:
          service:
            name: backend
            port:
              number: 80
```

## Persistent Volumes

### PersistentVolume (PV)

```yaml
apiVersion: v1
kind: PersistentVolume
metadata:
  name: data-pv
spec:
  capacity:
    storage: 100Gi
  accessModes:
  - ReadWriteOnce
  persistentVolumeReclaimPolicy: Retain
  storageClassName: fast-ssd
  hostPath:
    path: /data/pv
```

### PersistentVolumeClaim (PVC)

```yaml
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: data-pvc
  namespace: production
spec:
  accessModes:
  - ReadWriteOnce
  storageClassName: fast-ssd
  resources:
    requests:
      storage: 50Gi
```

**Using PVC in Pod:**
```yaml
apiVersion: v1
kind: Pod
metadata:
  name: myapp
spec:
  containers:
  - name: myapp
    image: myapp:1.0.0
    volumeMounts:
    - name: data
      mountPath: /app/data
  volumes:
  - name: data
    persistentVolumeClaim:
      claimName: data-pvc
```

## Resource Management Commands

### Get Resources

```bash
# Get all resources in namespace
kubectl get all

# Get specific resource types
kubectl get deployments
kubectl get pods
kubectl get services
kubectl get ingress

# Get with additional details
kubectl get pods -o wide
kubectl get pods -o yaml
kubectl get pods -o json

# Get with labels
kubectl get pods --show-labels
kubectl get pods -l app=myapp
kubectl get pods -l 'environment in (prod,staging)'

# Get from all namespaces
kubectl get pods --all-namespaces
kubectl get pods -A
```

### Create Resources

```bash
# Apply from file
kubectl apply -f deployment.yaml

# Apply from directory
kubectl apply -f ./manifests/

# Apply from URL
kubectl apply -f https://example.com/manifest.yaml

# Create from stdin
cat <<EOF | kubectl apply -f -
apiVersion: v1
kind: Namespace
metadata:
  name: test
EOF
```

### Update Resources

```bash
# Apply changes
kubectl apply -f deployment.yaml

# Edit resource interactively
kubectl edit deployment myapp

# Patch resource
kubectl patch deployment myapp -p '{"spec":{"replicas":5}}'

# Replace resource (delete and recreate)
kubectl replace -f deployment.yaml

# Set image
kubectl set image deployment/myapp myapp=myapp:v2.0.0
```

### Delete Resources

```bash
# Delete from file
kubectl delete -f deployment.yaml

# Delete by name
kubectl delete deployment myapp
kubectl delete pod myapp-pod

# Delete with label selector
kubectl delete pods -l app=myapp

# Delete all resources in namespace
kubectl delete all --all -n test

# Force delete stuck pod
kubectl delete pod myapp-pod --grace-period=0 --force
```

## Best Practices

### Resource Naming

- Use lowercase alphanumeric characters and hyphens
- Maximum 253 characters
- Start and end with alphanumeric character
- Use descriptive names: `web-frontend`, `api-backend`

### Labels

**Recommended labels:**
```yaml
metadata:
  labels:
    app.kubernetes.io/name: myapp
    app.kubernetes.io/instance: myapp-prod
    app.kubernetes.io/version: "1.0.0"
    app.kubernetes.io/component: backend
    app.kubernetes.io/part-of: myplatform
    app.kubernetes.io/managed-by: kubectl
    environment: production
    team: backend
```

### Annotations

**Common annotations:**
```yaml
metadata:
  annotations:
    description: "Backend API service"
    contact: "backend-team@example.com"
    prometheus.io/scrape: "true"
    prometheus.io/port: "8080"
    prometheus.io/path: "/metrics"
```

### Resource Requests and Limits

**Always set for production:**
```yaml
resources:
  requests:
    cpu: 100m      # Minimum guaranteed
    memory: 128Mi
  limits:
    cpu: 500m      # Maximum allowed
    memory: 512Mi
```

**Guidelines:**
- Set requests based on average usage
- Set limits 1.5-2x higher than requests
- Monitor actual usage with `kubectl top pods`
- Adjust based on metrics

### Health Probes

**Liveness Probe:** Restart container if unhealthy
**Readiness Probe:** Remove from service endpoints if not ready
**Startup Probe:** Allow slow-starting containers time to start

```yaml
livenessProbe:
  httpGet:
    path: /healthz
    port: 8080
  initialDelaySeconds: 30
  periodSeconds: 10
  timeoutSeconds: 5
  failureThreshold: 3

readinessProbe:
  httpGet:
    path: /ready
    port: 8080
  initialDelaySeconds: 5
  periodSeconds: 5
  timeoutSeconds: 3
  failureThreshold: 3

startupProbe:
  httpGet:
    path: /healthz
    port: 8080
  initialDelaySeconds: 0
  periodSeconds: 5
  timeoutSeconds: 3
  failureThreshold: 30  # 150 seconds to start
```

## Validation and Testing

```bash
# Validate YAML syntax
kubectl apply --dry-run=client -f manifest.yaml

# Server-side validation
kubectl apply --dry-run=server -f manifest.yaml

# Preview changes
kubectl diff -f manifest.yaml

# Validate with kube-linter (external tool)
kube-linter lint manifest.yaml

# Check resource compliance
kubectl-score manifest.yaml
```

## Resource Management Checklist

- [ ] Use declarative YAML manifests
- [ ] Set resource requests and limits
- [ ] Configure health probes
- [ ] Use appropriate labels and annotations
- [ ] Set security context (non-root user)
- [ ] Use Secrets for sensitive data
- [ ] Use ConfigMaps for configuration
- [ ] Define appropriate service type
- [ ] Set up Ingress for external access
- [ ] Use PVCs for persistent data
- [ ] Validate manifests before applying
- [ ] Version control all manifests
