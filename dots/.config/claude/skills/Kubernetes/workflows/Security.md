# Security Workflow

Kubernetes security best practices including RBAC, Pod Security Standards, Network Policies, and cluster hardening.

## When to Use

- Setting up RBAC for applications or users
- Implementing pod security controls
- Creating network segmentation
- Hardening cluster security
- Security auditing and compliance

## RBAC (Role-Based Access Control)

### Core Concepts

- **ServiceAccount**: Identity for pods
- **Role**: Permissions within a namespace
- **ClusterRole**: Cluster-wide permissions
- **RoleBinding**: Binds Role to users/groups/ServiceAccounts in namespace
- **ClusterRoleBinding**: Binds ClusterRole cluster-wide

### Create ServiceAccount

```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: myapp-sa
  namespace: production
```

### Create Role (Namespace-scoped)

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: pod-reader
  namespace: production
rules:
- apiGroups: [""]
  resources: ["pods"]
  verbs: ["get", "list", "watch"]
- apiGroups: [""]
  resources: ["pods/log"]
  verbs: ["get"]
```

### Create ClusterRole (Cluster-wide)

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRole
metadata:
  name: secret-reader
rules:
- apiGroups: [""]
  resources: ["secrets"]
  verbs: ["get", "list"]
```

### Create RoleBinding

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: read-pods
  namespace: production
subjects:
- kind: ServiceAccount
  name: myapp-sa
  namespace: production
roleRef:
  kind: Role
  name: pod-reader
  apiGroup: rbac.authorization.k8s.io
```

### Create ClusterRoleBinding

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRoleBinding
metadata:
  name: read-secrets-global
subjects:
- kind: ServiceAccount
  name: myapp-sa
  namespace: production
roleRef:
  kind: ClusterRole
  name: secret-reader
  apiGroup: rbac.authorization.k8s.io
```

### Common RBAC Patterns

**Read-only access to pods:**
```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: pod-reader
  namespace: production
rules:
- apiGroups: [""]
  resources: ["pods", "pods/log"]
  verbs: ["get", "list", "watch"]
```

**Deployment manager:**
```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: deployment-manager
  namespace: production
rules:
- apiGroups: ["apps"]
  resources: ["deployments", "replicasets"]
  verbs: ["get", "list", "watch", "create", "update", "patch", "delete"]
- apiGroups: [""]
  resources: ["pods"]
  verbs: ["get", "list", "watch"]
```

**ConfigMap and Secret manager:**
```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: config-manager
  namespace: production
rules:
- apiGroups: [""]
  resources: ["configmaps", "secrets"]
  verbs: ["get", "list", "create", "update", "patch"]
```

### Testing RBAC Permissions

```bash
# Check if current user can perform action
kubectl auth can-i create deployments
kubectl auth can-i get pods --namespace production

# Check permissions for specific user
kubectl auth can-i create deployments --as=user@example.com

# Check permissions for ServiceAccount
kubectl auth can-i list secrets --as=system:serviceaccount:production:myapp-sa

# Check all permissions for current user
kubectl auth can-i --list

# Check permissions for ServiceAccount in namespace
kubectl auth can-i --list --as=system:serviceaccount:production:myapp-sa -n production
```

### Using ServiceAccount in Pod

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: myapp
  namespace: production
spec:
  serviceAccountName: myapp-sa
  automountServiceAccountToken: true  # Default: true
  containers:
  - name: myapp
    image: myapp:1.0.0
```

## Pod Security Standards

### Pod Security Levels

1. **Privileged**: Unrestricted (for system components)
2. **Baseline**: Minimally restrictive (prevents known privilege escalations)
3. **Restricted**: Heavily restricted (security best practices)

### Pod Security Admission

**Namespace labels:**
```yaml
apiVersion: v1
kind: Namespace
metadata:
  name: production
  labels:
    pod-security.kubernetes.io/enforce: restricted
    pod-security.kubernetes.io/audit: restricted
    pod-security.kubernetes.io/warn: restricted
```

### Secure Pod Template

**Baseline security:**
```yaml
apiVersion: v1
kind: Pod
metadata:
  name: secure-pod
spec:
  securityContext:
    runAsNonRoot: true
    runAsUser: 1000
    fsGroup: 1000
  containers:
  - name: app
    image: myapp:1.0.0
    securityContext:
      allowPrivilegeEscalation: false
      capabilities:
        drop:
        - ALL
```

**Restricted security (production):**
```yaml
apiVersion: v1
kind: Pod
metadata:
  name: highly-secure-pod
spec:
  securityContext:
    runAsNonRoot: true
    runAsUser: 1000
    fsGroup: 1000
    seccompProfile:
      type: RuntimeDefault
  containers:
  - name: app
    image: myapp:1.0.0
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
  volumes:
  - name: tmp
    emptyDir: {}
```

### Security Context Fields

**Pod-level:**
- `runAsUser`: UID to run containers
- `runAsGroup`: GID to run containers
- `fsGroup`: Group for volume ownership
- `runAsNonRoot`: Prevent running as root
- `seccompProfile`: Seccomp profile
- `seLinuxOptions`: SELinux options

**Container-level:**
- `allowPrivilegeEscalation`: Prevent privilege escalation
- `readOnlyRootFilesystem`: Read-only root filesystem
- `runAsNonRoot`: Prevent running as root
- `runAsUser`: Override pod-level UID
- `capabilities`: Linux capabilities
- `seccompProfile`: Seccomp profile

## Network Policies

### Default Deny All Traffic

**IMPORTANT: Apply this first, then whitelist allowed traffic**

```yaml
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

### Allow Ingress from Specific Pods

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: allow-from-frontend
  namespace: production
spec:
  podSelector:
    matchLabels:
      app: backend
  policyTypes:
  - Ingress
  ingress:
  - from:
    - podSelector:
        matchLabels:
          app: frontend
    ports:
    - protocol: TCP
      port: 8080
```

### Allow Ingress from Specific Namespace

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: allow-from-monitoring
  namespace: production
spec:
  podSelector:
    matchLabels:
      app: backend
  policyTypes:
  - Ingress
  ingress:
  - from:
    - namespaceSelector:
        matchLabels:
          name: monitoring
    ports:
    - protocol: TCP
      port: 8080
```

### Allow Egress to Specific Services

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: allow-egress-to-database
  namespace: production
spec:
  podSelector:
    matchLabels:
      app: backend
  policyTypes:
  - Egress
  egress:
  - to:
    - podSelector:
        matchLabels:
          app: postgres
    ports:
    - protocol: TCP
      port: 5432
```

### Allow DNS and External HTTPS

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: allow-dns-and-https
  namespace: production
spec:
  podSelector:
    matchLabels:
      app: backend
  policyTypes:
  - Egress
  egress:
  # Allow DNS
  - to:
    - namespaceSelector:
        matchLabels:
          name: kube-system
      podSelector:
        matchLabels:
          k8s-app: kube-dns
    ports:
    - protocol: UDP
      port: 53
  # Allow external HTTPS
  - to:
    - ipBlock:
        cidr: 0.0.0.0/0
        except:
        - 169.254.169.254/32  # Block metadata service
    ports:
    - protocol: TCP
      port: 443
```

### Complete Network Policy Example

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: backend-network-policy
  namespace: production
spec:
  podSelector:
    matchLabels:
      app: backend
  policyTypes:
  - Ingress
  - Egress
  ingress:
  # Allow from frontend
  - from:
    - podSelector:
        matchLabels:
          app: frontend
    ports:
    - protocol: TCP
      port: 8080
  # Allow from ingress controller
  - from:
    - namespaceSelector:
        matchLabels:
          name: ingress-nginx
    ports:
    - protocol: TCP
      port: 8080
  egress:
  # Allow to database
  - to:
    - podSelector:
        matchLabels:
          app: postgres
    ports:
    - protocol: TCP
      port: 5432
  # Allow to Redis
  - to:
    - podSelector:
        matchLabels:
          app: redis
    ports:
    - protocol: TCP
      port: 6379
  # Allow DNS
  - to:
    - namespaceSelector:
        matchLabels:
          name: kube-system
      podSelector:
        matchLabels:
          k8s-app: kube-dns
    ports:
    - protocol: UDP
      port: 53
```

## Secrets Management

### Best Practices

1. **Never commit secrets to Git**
2. **Use volume mounts instead of environment variables**
3. **Enable encryption at rest**
4. **Use external secret management (Vault, AWS Secrets Manager)**
5. **Rotate secrets regularly**
6. **Limit secret access with RBAC**

### Using Secrets Securely

**BAD - Environment variables:**
```yaml
# Secrets visible in pod spec and logs
env:
- name: API_KEY
  valueFrom:
    secretKeyRef:
      name: myapp-secret
      key: api_key
```

**GOOD - Volume mounts:**
```yaml
volumeMounts:
- name: secrets
  mountPath: /etc/secrets
  readOnly: true
volumes:
- name: secrets
  secret:
    secretName: myapp-secret
    defaultMode: 0400
```

### External Secrets Operator

**CRD for syncing external secrets:**
```yaml
apiVersion: external-secrets.io/v1beta1
kind: SecretStore
metadata:
  name: aws-secrets-manager
  namespace: production
spec:
  provider:
    aws:
      service: SecretsManager
      region: us-east-1
      auth:
        jwt:
          serviceAccountRef:
            name: external-secrets-sa
---
apiVersion: external-secrets.io/v1beta1
kind: ExternalSecret
metadata:
  name: myapp-secret
  namespace: production
spec:
  refreshInterval: 1h
  secretStoreRef:
    name: aws-secrets-manager
    kind: SecretStore
  target:
    name: myapp-secret
    creationPolicy: Owner
  data:
  - secretKey: api_key
    remoteRef:
      key: prod/myapp/api_key
```

### Sealed Secrets

**Encrypt secrets for Git storage:**
```bash
# Install kubeseal
# On NixOS: pkgs.kubeseal

# Encrypt secret
kubectl create secret generic myapp-secret \
  --from-literal=api_key=supersecret \
  --dry-run=client -o yaml | \
  kubeseal -o yaml > sealed-secret.yaml

# Commit sealed-secret.yaml to Git
git add sealed-secret.yaml

# Apply (controller decrypts)
kubectl apply -f sealed-secret.yaml
```

## Image Security

### Use Specific Tags

**BAD:**
```yaml
image: nginx:latest
```

**GOOD:**
```yaml
image: nginx:1.25.3-alpine
```

### Image Pull Policies

```yaml
spec:
  containers:
  - name: app
    image: myapp:1.0.0
    imagePullPolicy: IfNotPresent  # Or Always, Never
```

**Policies:**
- `IfNotPresent`: Pull if not cached (default for tagged images)
- `Always`: Always pull (default for :latest)
- `Never`: Never pull, must be cached

### Private Registry Authentication

```bash
# Create docker-registry secret
kubectl create secret docker-registry regcred \
  --docker-server=myregistry.com \
  --docker-username=user \
  --docker-password=pass \
  --docker-email=user@example.com
```

```yaml
spec:
  imagePullSecrets:
  - name: regcred
```

### Image Scanning

**Trivy (vulnerability scanner):**
```bash
# Scan image
trivy image myapp:1.0.0

# Scan with severity threshold
trivy image --severity HIGH,CRITICAL myapp:1.0.0

# Scan Kubernetes manifests
trivy config deployment.yaml
```

## Cluster Hardening

### API Server Security

**Recommended flags:**
```yaml
- --anonymous-auth=false
- --enable-admission-plugins=NodeRestriction,PodSecurityPolicy
- --audit-log-path=/var/log/audit.log
- --audit-log-maxage=30
- --enable-bootstrap-token-auth=false
- --insecure-port=0
```

### Enable Audit Logging

```yaml
apiVersion: audit.k8s.io/v1
kind: Policy
rules:
- level: Metadata
  resources:
  - group: ""
    resources: ["secrets", "configmaps"]
- level: RequestResponse
  resources:
  - group: ""
    resources: ["pods"]
  verbs: ["create", "update", "patch", "delete"]
```

### Kubelet Security

**Recommended flags:**
```
--anonymous-auth=false
--authorization-mode=Webhook
--read-only-port=0
--protect-kernel-defaults=true
```

## Security Scanning and Compliance

### kube-bench (CIS Benchmark)

```bash
# Run CIS Kubernetes benchmark
kubectl apply -f https://raw.githubusercontent.com/aquasecurity/kube-bench/main/job.yaml

# View results
kubectl logs -l app=kube-bench
```

### kube-hunter (Penetration Testing)

```bash
# Run security assessment
kubectl apply -f https://raw.githubusercontent.com/aquasecurity/kube-hunter/main/job.yaml

# View results
kubectl logs -l app=kube-hunter
```

### Falco (Runtime Security)

**Detect anomalous activity:**
```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: falco-rules
  namespace: falco
data:
  custom-rules.yaml: |
    - rule: Unauthorized Process in Container
      desc: Detect unauthorized process execution
      condition: spawned_process and container and not proc.name in (allowed_processes)
      output: Unauthorized process in container (user=%user.name command=%proc.cmdline)
      priority: WARNING
```

## Security Checklist

### Pod Security
- [ ] Run as non-root user (`runAsNonRoot: true`)
- [ ] Use read-only root filesystem (`readOnlyRootFilesystem: true`)
- [ ] Drop all capabilities (`capabilities.drop: [ALL]`)
- [ ] Disable privilege escalation (`allowPrivilegeEscalation: false`)
- [ ] Set seccomp profile (`seccompProfile.type: RuntimeDefault`)
- [ ] Define resource limits
- [ ] Use specific image tags (not :latest)

### RBAC
- [ ] Use ServiceAccounts for applications
- [ ] Follow principle of least privilege
- [ ] Use Role instead of ClusterRole when possible
- [ ] Regularly audit RBAC permissions
- [ ] Disable auto-mounting of ServiceAccount tokens if not needed

### Network Security
- [ ] Implement default-deny network policies
- [ ] Whitelist required traffic only
- [ ] Segment namespaces with network policies
- [ ] Use TLS for in-cluster communication
- [ ] Block access to cloud metadata service

### Secrets
- [ ] Use external secret management (Vault, AWS Secrets Manager)
- [ ] Mount secrets as volumes, not environment variables
- [ ] Enable encryption at rest for secrets
- [ ] Rotate secrets regularly
- [ ] Limit secret access with RBAC

### Cluster Hardening
- [ ] Enable audit logging
- [ ] Disable anonymous authentication
- [ ] Use Pod Security Standards
- [ ] Keep Kubernetes version up to date
- [ ] Run CIS benchmark (kube-bench)
- [ ] Implement admission controllers
- [ ] Use private nodes (no public IPs)

### Image Security
- [ ] Scan images for vulnerabilities
- [ ] Use minimal base images (alpine, distroless)
- [ ] Use specific image tags
- [ ] Sign and verify images
- [ ] Use private registry with authentication

### Monitoring
- [ ] Enable audit logs
- [ ] Monitor with Falco or similar
- [ ] Set up alerts for security events
- [ ] Regularly review logs
- [ ] Implement intrusion detection

## Example: Secure Application Deployment

```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: secure-app
  namespace: production
---
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: secure-app-role
  namespace: production
rules:
- apiGroups: [""]
  resources: ["configmaps"]
  resourceNames: ["app-config"]
  verbs: ["get"]
---
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: secure-app-binding
  namespace: production
subjects:
- kind: ServiceAccount
  name: secure-app
  namespace: production
roleRef:
  kind: Role
  name: secure-app-role
  apiGroup: rbac.authorization.k8s.io
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: secure-app
  namespace: production
spec:
  replicas: 3
  selector:
    matchLabels:
      app: secure-app
  template:
    metadata:
      labels:
        app: secure-app
    spec:
      serviceAccountName: secure-app
      automountServiceAccountToken: true
      securityContext:
        runAsNonRoot: true
        runAsUser: 1000
        fsGroup: 1000
        seccompProfile:
          type: RuntimeDefault
      containers:
      - name: app
        image: myregistry.com/secure-app:1.0.0
        imagePullPolicy: IfNotPresent
        securityContext:
          allowPrivilegeEscalation: false
          readOnlyRootFilesystem: true
          runAsNonRoot: true
          runAsUser: 1000
          capabilities:
            drop:
            - ALL
        resources:
          requests:
            cpu: 100m
            memory: 128Mi
          limits:
            cpu: 500m
            memory: 512Mi
        volumeMounts:
        - name: tmp
          mountPath: /tmp
        - name: secrets
          mountPath: /etc/secrets
          readOnly: true
      volumes:
      - name: tmp
        emptyDir: {}
      - name: secrets
        secret:
          secretName: app-secret
          defaultMode: 0400
      imagePullSecrets:
      - name: regcred
---
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: secure-app-netpol
  namespace: production
spec:
  podSelector:
    matchLabels:
      app: secure-app
  policyTypes:
  - Ingress
  - Egress
  ingress:
  - from:
    - podSelector:
        matchLabels:
          app: frontend
    ports:
    - protocol: TCP
      port: 8080
  egress:
  - to:
    - namespaceSelector:
        matchLabels:
          name: kube-system
      podSelector:
        matchLabels:
          k8s-app: kube-dns
    ports:
    - protocol: UDP
      port: 53
```
