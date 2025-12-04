# Scaling Workflow

Kubernetes scaling strategies including manual scaling, Horizontal Pod Autoscaling (HPA), and Vertical Pod Autoscaling (VPA).

## When to Use

- Scaling applications based on load
- Setting up autoscaling policies
- Optimizing resource utilization
- Handling traffic spikes

## Manual Scaling

### Scale Deployment

```bash
# Scale to specific number of replicas
kubectl scale deployment myapp --replicas=5

# Scale multiple deployments
kubectl scale deployment myapp myapp2 --replicas=3

# Scale with label selector
kubectl scale deployment -l app=myapp --replicas=3

# Scale replica set
kubectl scale rs myapp-rs --replicas=5

# Scale stateful set
kubectl scale statefulset postgres --replicas=3
```

### Verify Scaling

```bash
# Check deployment status
kubectl get deployment myapp

# Watch scaling progress
kubectl get pods -l app=myapp -w

# Check replica set
kubectl get rs -l app=myapp
```

## Horizontal Pod Autoscaler (HPA)

### Prerequisites

**Install metrics-server:**
```bash
kubectl apply -f https://github.com/kubernetes-sigs/metrics-server/releases/latest/download/components.yaml
```

**Verify metrics-server:**
```bash
kubectl get deployment metrics-server -n kube-system
kubectl top nodes
kubectl top pods
```

### Create HPA (CPU-based)

**Imperative:**
```bash
# Create HPA based on CPU utilization
kubectl autoscale deployment myapp --cpu-percent=70 --min=2 --max=10

# View HPA
kubectl get hpa myapp

# Describe HPA
kubectl describe hpa myapp
```

**Declarative:**
```yaml
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: myapp-hpa
  namespace: production
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: myapp
  minReplicas: 2
  maxReplicas: 10
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
```

### HPA with Multiple Metrics

```yaml
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: myapp-hpa
  namespace: production
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: myapp
  minReplicas: 2
  maxReplicas: 20
  behavior:
    scaleDown:
      stabilizationWindowSeconds: 300
      policies:
      - type: Percent
        value: 50
        periodSeconds: 60
      - type: Pods
        value: 2
        periodSeconds: 60
      selectPolicy: Min
    scaleUp:
      stabilizationWindowSeconds: 0
      policies:
      - type: Percent
        value: 100
        periodSeconds: 30
      - type: Pods
        value: 4
        periodSeconds: 30
      selectPolicy: Max
  metrics:
  # CPU utilization
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  # Memory utilization
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
  # Custom metric (requests per second)
  - type: Pods
    pods:
      metric:
        name: http_requests_per_second
      target:
        type: AverageValue
        averageValue: "1000"
```

### HPA Based on Custom Metrics

**Prometheus adapter example:**
```yaml
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: myapp-hpa-custom
  namespace: production
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: myapp
  minReplicas: 2
  maxReplicas: 10
  metrics:
  - type: Pods
    pods:
      metric:
        name: http_requests_per_second
      target:
        type: AverageValue
        averageValue: "1000"
  - type: Pods
    pods:
      metric:
        name: queue_length
      target:
        type: AverageValue
        averageValue: "30"
```

### Monitor HPA

```bash
# Get HPA status
kubectl get hpa

# Watch HPA in real-time
kubectl get hpa -w

# Describe HPA for details
kubectl describe hpa myapp-hpa

# View events
kubectl get events --sort-by=.metadata.creationTimestamp | grep HorizontalPodAutoscaler

# Check current metrics
kubectl top pods -l app=myapp
```

### Test HPA

**Load testing:**
```bash
# Generate load with kubectl run
kubectl run -i --tty load-generator --rm --image=busybox --restart=Never -- /bin/sh -c "while sleep 0.01; do wget -q -O- http://myapp; done"

# Watch HPA scale up
kubectl get hpa myapp-hpa -w

# Watch pods being created
kubectl get pods -l app=myapp -w
```

### Delete HPA

```bash
# Delete HPA
kubectl delete hpa myapp-hpa

# Deployment will maintain last replica count
kubectl scale deployment myapp --replicas=3
```

## Vertical Pod Autoscaler (VPA)

### Install VPA

```bash
# Clone VPA repository
git clone https://github.com/kubernetes/autoscaler.git
cd autoscaler/vertical-pod-autoscaler

# Install VPA
./hack/vpa-up.sh

# Verify installation
kubectl get deployment -n kube-system | grep vpa
```

### Create VPA

**Recommendation mode (does not modify pods):**
```yaml
apiVersion: autoscaling.k8s.io/v1
kind: VerticalPodAutoscaler
metadata:
  name: myapp-vpa
  namespace: production
spec:
  targetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: myapp
  updatePolicy:
    updateMode: "Off"  # Recommendation only
```

**Auto mode (updates pods):**
```yaml
apiVersion: autoscaling.k8s.io/v1
kind: VerticalPodAutoscaler
metadata:
  name: myapp-vpa
  namespace: production
spec:
  targetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: myapp
  updatePolicy:
    updateMode: "Auto"
  resourcePolicy:
    containerPolicies:
    - containerName: myapp
      minAllowed:
        cpu: 100m
        memory: 128Mi
      maxAllowed:
        cpu: 2000m
        memory: 4Gi
      controlledResources: ["cpu", "memory"]
```

**Initial mode (sets resources on pod creation only):**
```yaml
apiVersion: autoscaling.k8s.io/v1
kind: VerticalPodAutoscaler
metadata:
  name: myapp-vpa
  namespace: production
spec:
  targetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: myapp
  updatePolicy:
    updateMode: "Initial"
```

### View VPA Recommendations

```bash
# Get VPA status
kubectl get vpa myapp-vpa

# Describe VPA for recommendations
kubectl describe vpa myapp-vpa

# View recommendations in YAML
kubectl get vpa myapp-vpa -o yaml
```

**Example output:**
```yaml
recommendation:
  containerRecommendations:
  - containerName: myapp
    lowerBound:
      cpu: 150m
      memory: 256Mi
    target:
      cpu: 200m
      memory: 512Mi
    uncappedTarget:
      cpu: 250m
      memory: 768Mi
    upperBound:
      cpu: 400m
      memory: 1Gi
```

## Cluster Autoscaler

### Cloud Provider Setup

**AWS (EKS):**
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: cluster-autoscaler
  namespace: kube-system
spec:
  replicas: 1
  selector:
    matchLabels:
      app: cluster-autoscaler
  template:
    metadata:
      labels:
        app: cluster-autoscaler
    spec:
      serviceAccountName: cluster-autoscaler
      containers:
      - image: k8s.gcr.io/autoscaling/cluster-autoscaler:v1.27.0
        name: cluster-autoscaler
        command:
        - ./cluster-autoscaler
        - --v=4
        - --stderrthreshold=info
        - --cloud-provider=aws
        - --skip-nodes-with-local-storage=false
        - --expander=least-waste
        - --node-group-auto-discovery=asg:tag=k8s.io/cluster-autoscaler/enabled,k8s.io/cluster-autoscaler/my-cluster
        - --balance-similar-node-groups
        - --skip-nodes-with-system-pods=false
```

**GKE (automatically enabled):**
```bash
# Enable cluster autoscaler
gcloud container clusters update my-cluster \
  --enable-autoscaling \
  --min-nodes=1 \
  --max-nodes=10 \
  --zone=us-central1-a
```

### Pod Disruption Budget (PDB)

**Protect pods during scaling:**
```yaml
apiVersion: policy/v1
kind: PodDisruptionBudget
metadata:
  name: myapp-pdb
  namespace: production
spec:
  minAvailable: 2  # Or use maxUnavailable: 1
  selector:
    matchLabels:
      app: myapp
```

**With percentage:**
```yaml
apiVersion: policy/v1
kind: PodDisruptionBudget
metadata:
  name: myapp-pdb
  namespace: production
spec:
  maxUnavailable: 25%
  selector:
    matchLabels:
      app: myapp
```

## Scaling Best Practices

### Resource Requests and Limits

**Required for HPA:**
```yaml
resources:
  requests:
    cpu: 100m      # HPA uses this for percentage calculation
    memory: 128Mi
  limits:
    cpu: 500m
    memory: 512Mi
```

### HPA Configuration

**Recommended settings:**
- `minReplicas`: 2+ for high availability
- `maxReplicas`: 10-20x minReplicas
- CPU target: 60-80% utilization
- Memory target: 70-90% utilization
- Scale-down stabilization: 300 seconds (5 minutes)
- Scale-up stabilization: 0-60 seconds

### Scaling Behavior

**Conservative scaling:**
```yaml
behavior:
  scaleDown:
    stabilizationWindowSeconds: 300  # Wait 5 minutes before scaling down
    policies:
    - type: Percent
      value: 25  # Scale down by max 25% at a time
      periodSeconds: 60
  scaleUp:
    stabilizationWindowSeconds: 60
    policies:
    - type: Pods
      value: 2  # Add max 2 pods at a time
      periodSeconds: 30
```

**Aggressive scaling:**
```yaml
behavior:
  scaleDown:
    stabilizationWindowSeconds: 60
    policies:
    - type: Percent
      value: 50
      periodSeconds: 30
  scaleUp:
    stabilizationWindowSeconds: 0
    policies:
    - type: Percent
      value: 100  # Double pods each time
      periodSeconds: 15
```

### Anti-Patterns to Avoid

1. **No resource requests** - HPA cannot calculate metrics
2. **Too aggressive scaling** - Causes thrashing
3. **VPA + HPA on same metric** - Conflicts
4. **No PodDisruptionBudget** - Unsafe during scaling
5. **Single replica minimum** - No high availability
6. **No monitoring** - Cannot tune scaling

## Scaling Strategies

### Predictive Scaling

**Schedule-based autoscaling (CronJob pattern):**
```yaml
apiVersion: batch/v1
kind: CronJob
metadata:
  name: scale-up-morning
  namespace: production
spec:
  schedule: "0 8 * * 1-5"  # 8 AM Monday-Friday
  jobTemplate:
    spec:
      template:
        spec:
          serviceAccountName: scaler
          restartPolicy: OnFailure
          containers:
          - name: kubectl
            image: bitnami/kubectl:latest
            command:
            - kubectl
            - scale
            - deployment/myapp
            - --replicas=10
---
apiVersion: batch/v1
kind: CronJob
metadata:
  name: scale-down-evening
  namespace: production
spec:
  schedule: "0 18 * * 1-5"  # 6 PM Monday-Friday
  jobTemplate:
    spec:
      template:
        spec:
          serviceAccountName: scaler
          restartPolicy: OnFailure
          containers:
          - name: kubectl
            image: bitnami/kubectl:latest
            command:
            - kubectl
            - scale
            - deployment/myapp
            - --replicas=3
```

### KEDA (Kubernetes Event-Driven Autoscaling)

**Scale based on external metrics:**
```yaml
apiVersion: keda.sh/v1alpha1
kind: ScaledObject
metadata:
  name: myapp-scaledobject
  namespace: production
spec:
  scaleTargetRef:
    name: myapp
  minReplicaCount: 2
  maxReplicaCount: 20
  triggers:
  # Scale based on Prometheus metric
  - type: prometheus
    metadata:
      serverAddress: http://prometheus:9090
      metricName: http_requests_per_second
      threshold: '1000'
      query: sum(rate(http_requests_total[2m]))
  # Scale based on message queue
  - type: rabbitmq
    metadata:
      queueName: tasks
      queueLength: '50'
      connectionFromEnv: RABBITMQ_URL
```

## Monitoring Scaling

### Metrics to Track

```bash
# Current resource usage
kubectl top pods -l app=myapp
kubectl top nodes

# HPA status
kubectl get hpa myapp-hpa

# Replica count over time
kubectl get deployment myapp -w

# Events
kubectl get events --sort-by=.metadata.creationTimestamp | grep -i scale
```

### Prometheus Queries

**Pod count:**
```promql
kube_deployment_status_replicas{deployment="myapp"}
```

**CPU utilization:**
```promql
rate(container_cpu_usage_seconds_total{pod=~"myapp-.*"}[5m])
```

**Memory usage:**
```promql
container_memory_usage_bytes{pod=~"myapp-.*"}
```

**HPA target metric:**
```promql
kube_horizontalpodautoscaler_status_current_replicas{horizontalpodautoscaler="myapp-hpa"}
kube_horizontalpodautoscaler_status_desired_replicas{horizontalpodautoscaler="myapp-hpa"}
```

## Troubleshooting Scaling

### HPA Not Scaling

**Check metrics availability:**
```bash
kubectl get --raw /apis/metrics.k8s.io/v1beta1/nodes
kubectl get --raw /apis/metrics.k8s.io/v1beta1/pods

kubectl top nodes
kubectl top pods -l app=myapp
```

**Check resource requests:**
```bash
kubectl get deployment myapp -o yaml | grep -A 5 resources
```

**Check HPA status:**
```bash
kubectl describe hpa myapp-hpa

# Look for:
# - Unable to get metrics
# - Failed to compute desired number of replicas
# - Resource requests not set
```

### VPA Not Working

**Check VPA installation:**
```bash
kubectl get deployment -n kube-system | grep vpa
```

**Check VPA status:**
```bash
kubectl describe vpa myapp-vpa
```

### Cluster Autoscaler Issues

**Check logs:**
```bash
kubectl logs -n kube-system -l app=cluster-autoscaler
```

**Check node status:**
```bash
kubectl get nodes
kubectl describe nodes
```

## Scaling Checklist

- [ ] Resource requests and limits defined
- [ ] Metrics-server installed and working
- [ ] HPA configured with appropriate thresholds
- [ ] MinReplicas >= 2 for high availability
- [ ] PodDisruptionBudget configured
- [ ] Scaling behavior tuned for workload
- [ ] Monitoring and alerting set up
- [ ] Load testing performed
- [ ] Cost implications considered
- [ ] Documentation updated
