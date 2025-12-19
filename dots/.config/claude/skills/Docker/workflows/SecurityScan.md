# SecurityScan Workflow

Scan container images for security vulnerabilities, misconfigurations, and best practice violations.

## Process

1. **Detect Runtime**
   - Run `DetectRuntime.sh` to determine Docker/Podman

2. **Select Scanning Tool**
   - Trivy (recommended, comprehensive)
   - Docker Scout (built-in Docker)
   - Grype (Anchore)
   - Clair
   - Snyk

3. **Scan Image**
   - Scan for vulnerabilities (CVEs)
   - Check for secrets
   - Analyze configuration
   - Check for malware

4. **Review Results**
   - Categorize findings by severity
   - Identify actionable items
   - Generate report

5. **Remediate**
   - Update base images
   - Patch dependencies
   - Remove secrets
   - Fix misconfigurations

## Trivy (Recommended)

### Installation

```bash
# Using package manager
# Debian/Ubuntu
sudo apt install trivy

# Fedora/RHEL
sudo dnf install trivy

# macOS
brew install trivy

# Or use container
alias trivy="docker run --rm -v /var/run/docker.sock:/var/run/docker.sock aquasec/trivy"
```

### Scan Images

**Basic scan:**
```bash
trivy image myapp:latest
trivy image nginx:latest
```

**Scan with specific severity:**
```bash
trivy image --severity HIGH,CRITICAL myapp:latest
```

**Scan for specific vulnerability types:**
```bash
# OS packages only
trivy image --vuln-type os myapp:latest

# Application dependencies only
trivy image --vuln-type library myapp:latest

# Both
trivy image --vuln-type os,library myapp:latest
```

**Output formats:**
```bash
# Table (default)
trivy image myapp:latest

# JSON
trivy image -f json myapp:latest

# SARIF (for GitHub)
trivy image -f sarif -o results.sarif myapp:latest

# Template
trivy image -f template --template "@contrib/html.tpl" -o report.html myapp:latest
```

**Scan filesystem:**
```bash
trivy fs /path/to/project
trivy fs --security-checks vuln,secret,config .
```

**Scan Dockerfile:**
```bash
trivy config Dockerfile
```

**Scan Kubernetes manifests:**
```bash
trivy config deployment.yaml
```

### Advanced Trivy

**Ignore unfixed vulnerabilities:**
```bash
trivy image --ignore-unfixed myapp:latest
```

**Use .trivyignore file:**
```bash
# .trivyignore
CVE-2023-1234
CVE-2023-5678
```

**Scan with cache:**
```bash
trivy image --cache-dir /tmp/trivy myapp:latest
```

**Exit with error on vulnerabilities:**
```bash
trivy image --exit-code 1 --severity CRITICAL myapp:latest
```

**Scan remote image:**
```bash
trivy image ghcr.io/username/myapp:latest
```

## Docker Scout (Built-in)

### Enable Docker Scout

```bash
# Login
docker login

# Enable Scout
docker scout enroll
```

### Scan Images

**Quick scan:**
```bash
docker scout quickview myapp:latest
```

**CVE scan:**
```bash
docker scout cves myapp:latest
```

**Show only fixable:**
```bash
docker scout cves --only-fixed myapp:latest
```

**Compare images:**
```bash
docker scout compare myapp:old myapp:new
```

**Recommendations:**
```bash
docker scout recommendations myapp:latest
```

**Export to SARIF:**
```bash
docker scout cves --format sarif myapp:latest > results.sarif
```

## Grype (Anchore)

### Installation

```bash
# Using package manager
brew install grype

# Or download binary
curl -sSfL https://raw.githubusercontent.com/anchore/grype/main/install.sh | sh

# Or use container
alias grype="docker run --rm -v /var/run/docker.sock:/var/run/docker.sock anchore/grype"
```

### Scan Images

**Basic scan:**
```bash
grype myapp:latest
```

**Scan with specific severity:**
```bash
grype myapp:latest --only-fixed
grype myapp:latest --fail-on critical
```

**Output formats:**
```bash
grype -o json myapp:latest
grype -o table myapp:latest
grype -o cyclonedx myapp:latest
```

## Snyk Container

### Installation

```bash
# Install Snyk CLI
npm install -g snyk

# Or use binary
curl -O https://static.snyk.io/cli/latest/snyk-linux
chmod +x snyk-linux
mv snyk-linux /usr/local/bin/snyk

# Authenticate
snyk auth
```

### Scan Images

**Scan image:**
```bash
snyk container test myapp:latest
```

**Monitor for vulnerabilities:**
```bash
snyk container monitor myapp:latest
```

**Test and generate report:**
```bash
snyk container test myapp:latest --json-file-output=results.json
```

## Clair

### Using Clair

**Run Clair server:**
```bash
docker run -p 6060:6060 -p 6061:6061 -d --name clair quay.io/coreos/clair:latest
```

**Scan with clairctl:**
```bash
clairctl analyze myapp:latest
clairctl report myapp:latest
```

## Secret Scanning

### Trivy Secret Scan

**Scan for secrets:**
```bash
trivy fs --security-checks secret .
trivy image --security-checks secret myapp:latest
```

### TruffleHog

```bash
# Install
pip install truffleHog

# Scan filesystem
trufflehog filesystem /path/to/project

# Scan container image
docker save myapp:latest | trufflehog
```

### git-secrets

```bash
# Install
git clone https://github.com/awslabs/git-secrets.git
cd git-secrets && make install

# Scan
git secrets --scan
```

## Configuration Scanning

### Hadolint (Dockerfile Linter)

**Install:**
```bash
brew install hadolint

# Or use container
docker run --rm -i hadolint/hadolint < Dockerfile
```

**Scan Dockerfile:**
```bash
hadolint Dockerfile
hadolint --ignore DL3008 Dockerfile  # Ignore specific rule
```

### Docker Bench Security

**Run security audit:**
```bash
docker run --rm --net host --pid host --userns host --cap-add audit_control \
  -v /var/lib:/var/lib \
  -v /var/run/docker.sock:/var/run/docker.sock \
  -v /etc:/etc \
  --label docker_bench_security \
  docker/docker-bench-security
```

## CI/CD Integration

### GitHub Actions

```yaml
name: Container Scan

on: [push, pull_request]

jobs:
  scan:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Build image
        run: docker build -t myapp:${{ github.sha }} .

      - name: Run Trivy scanner
        uses: aquasecurity/trivy-action@master
        with:
          image-ref: myapp:${{ github.sha }}
          format: 'sarif'
          output: 'trivy-results.sarif'

      - name: Upload Trivy results to GitHub Security
        uses: github/codeql-action/upload-sarif@v2
        with:
          sarif_file: 'trivy-results.sarif'
```

### GitLab CI

```yaml
container_scan:
  image: aquasec/trivy:latest
  script:
    - trivy image --format json --output results.json myapp:latest
  artifacts:
    reports:
      container_scanning: results.json
```

### Jenkins

```groovy
pipeline {
    agent any
    stages {
        stage('Build') {
            steps {
                sh 'docker build -t myapp:latest .'
            }
        }
        stage('Security Scan') {
            steps {
                sh 'trivy image --exit-code 1 --severity CRITICAL myapp:latest'
            }
        }
    }
}
```

## Best Practices

### Regular Scanning

- Scan during build
- Scan before push
- Scan in CI/CD pipeline
- Scan running containers periodically
- Monitor for new vulnerabilities

### Prioritization

1. **Critical**: Fix immediately
2. **High**: Fix within 1 week
3. **Medium**: Fix within 1 month
4. **Low**: Fix when convenient

### Base Image Selection

```dockerfile
# Prefer minimal base images
FROM alpine:3.19
FROM debian:bookworm-slim
FROM gcr.io/distroless/static-debian12

# Avoid
FROM ubuntu:latest  # Too large, "latest" is unpredictable
```

### Multi-stage Builds

```dockerfile
# Build stage with more tools
FROM golang:1.21 AS builder
WORKDIR /src
COPY . .
RUN go build -o app

# Runtime stage with minimal image
FROM gcr.io/distroless/static-debian12
COPY --from=builder /src/app /app
ENTRYPOINT ["/app"]
```

### Keep Images Updated

```bash
# Update base images regularly
docker pull alpine:3.19
docker build --no-cache -t myapp:latest .
```

### Don't Ignore Vulnerabilities

- Understand each vulnerability
- Check if it applies to your use case
- Document why ignoring (if appropriate)
- Revisit ignored vulnerabilities periodically

## Interpreting Results

### Vulnerability Severity

**CVSS Score:**
- **Critical** (9.0-10.0): Immediate action required
- **High** (7.0-8.9): Fix soon
- **Medium** (4.0-6.9): Schedule fix
- **Low** (0.1-3.9): Fix opportunistically

### False Positives

**Common scenarios:**
- Vulnerability in unused code path
- Vulnerability requires conditions that don't exist
- Already mitigated at runtime

**Handle false positives:**
```bash
# Document in .trivyignore
echo "CVE-2023-1234 # False positive: feature not used" >> .trivyignore
```

## Remediation Strategies

### Update Dependencies

**For OS packages:**
```dockerfile
# Update packages during build
RUN apt-get update && \
    apt-get upgrade -y && \
    rm -rf /var/lib/apt/lists/*
```

**For application dependencies:**
```dockerfile
# Update Go dependencies
RUN go get -u && go mod tidy

# Update Node dependencies
RUN npm update

# Update Python dependencies
RUN pip install --upgrade -r requirements.txt
```

### Use Specific Versions

```dockerfile
# Bad: unpredictable
FROM python:latest

# Good: specific and minimal
FROM python:3.11-slim-bookworm
```

### Remove Unnecessary Packages

```dockerfile
# Install only what's needed
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        package1 \
        package2 && \
    rm -rf /var/lib/apt/lists/*
```

### Use Distroless Images

```dockerfile
FROM gcr.io/distroless/python3-debian12
COPY --from=builder /app /app
CMD ["/app/main.py"]
```

## Common Vulnerabilities

### Secrets in Images

**Problem:**
```dockerfile
# Bad: secrets in image
ENV API_KEY="secret123"
COPY .env /app/.env
```

**Solution:**
```dockerfile
# Good: secrets at runtime
# Use environment variables or secrets management
```

### Running as Root

**Problem:**
```dockerfile
# Bad: default user is root
CMD ["/app"]
```

**Solution:**
```dockerfile
# Good: create and use non-root user
RUN adduser -D -u 1000 appuser
USER appuser
CMD ["/app"]
```

### Exposed Sensitive Ports

**Check for:**
- Database ports (3306, 5432)
- Admin interfaces
- Debug ports

### Old Base Images

**Problem:**
```dockerfile
FROM ubuntu:18.04  # EOL, no security updates
```

**Solution:**
```dockerfile
FROM ubuntu:22.04  # Supported LTS version
```

## Output Format

After security scan:
```
✓ Security scan completed
  Image: myapp:latest
  Scanner: Trivy

  Vulnerabilities found:
    Critical: 2
    High: 5
    Medium: 12
    Low: 8

  Top issues:
    1. CVE-2023-1234 (Critical) - OpenSSL vulnerability
       Fix: Update to openssl 3.0.10
    2. CVE-2023-5678 (High) - curl buffer overflow
       Fix: Update to curl 8.1.2

  Secrets found: 0
  Misconfigurations: 3

  Recommendations:
    - Update base image from alpine:3.17 to alpine:3.19
    - Remove unnecessary package: wget
    - Add non-root user

  Full report: results.json
```

## Quick Reference

```bash
# Trivy
trivy image myapp:latest
trivy image --severity CRITICAL,HIGH myapp:latest
trivy fs --security-checks vuln,secret,config .

# Docker Scout
docker scout cves myapp:latest
docker scout quickview myapp:latest

# Grype
grype myapp:latest

# Snyk
snyk container test myapp:latest

# Hadolint (Dockerfile)
hadolint Dockerfile
```
