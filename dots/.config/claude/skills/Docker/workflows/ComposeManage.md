# ComposeManage Workflow

Manage multi-container applications with Docker Compose or Podman Compose.

## Process

1. **Detect Runtime and Compose Tool**
   - Run `DetectRuntime.sh` to determine available compose tool
   - Check for `docker-compose`, `docker compose`, or `podman-compose`

2. **Analyze Compose File**
   - Check for `docker-compose.yml` or `compose.yaml`
   - Validate compose file syntax
   - Review services, networks, volumes defined

3. **Determine Action**
   - Start services (up)
   - Stop services (down)
   - Restart services
   - View logs
   - Scale services
   - Build services

4. **Execute Compose Command**
   - Run appropriate compose command
   - Handle environment variables
   - Manage profiles if used

5. **Verify State**
   - Check service status
   - Show running services
   - Display relevant logs

## Common Operations

### Start Services

**Start all services:**
```bash
docker-compose up -d
docker compose up -d
podman-compose up -d
```

**Start specific service:**
```bash
docker-compose up -d web
docker compose up -d web
podman-compose up -d web
```

**Start with build:**
```bash
docker-compose up -d --build
docker compose up -d --build
podman-compose up -d --build
```

**Start with specific file:**
```bash
docker-compose -f docker-compose.prod.yml up -d
docker compose -f docker-compose.prod.yml up -d
podman-compose -f docker-compose.prod.yml up -d
```

### Stop Services

**Stop all services:**
```bash
docker-compose down
docker compose down
podman-compose down
```

**Stop and remove volumes:**
```bash
docker-compose down -v
docker compose down -v
podman-compose down -v
```

**Stop specific service:**
```bash
docker-compose stop web
docker compose stop web
podman-compose stop web
```

### View Status and Logs

**List running services:**
```bash
docker-compose ps
docker compose ps
podman-compose ps
```

**View logs:**
```bash
docker-compose logs
docker compose logs
podman-compose logs
```

**Follow logs:**
```bash
docker-compose logs -f
docker compose logs -f
podman-compose logs -f
```

**Logs for specific service:**
```bash
docker-compose logs web
docker compose logs web
podman-compose logs web
```

### Build and Rebuild

**Build all services:**
```bash
docker-compose build
docker compose build
podman-compose build
```

**Build specific service:**
```bash
docker-compose build web
docker compose build web
podman-compose build web
```

**Build with no cache:**
```bash
docker-compose build --no-cache
docker compose build --no-cache
podman-compose build --no-cache
```

### Scale Services

**Scale service to multiple instances:**
```bash
docker-compose up -d --scale web=3
docker compose up -d --scale web=3
podman-compose up -d --scale web=3
```

### Execute Commands

**Run command in service:**
```bash
docker-compose exec web bash
docker compose exec web bash
podman-compose exec web bash
```

**Run one-off command:**
```bash
docker-compose run web python manage.py migrate
docker compose run web python manage.py migrate
podman-compose run web python manage.py migrate
```

## Compose File Best Practices

### Structure
```yaml
version: '3.8'

services:
  web:
    build:
      context: .
      dockerfile: Dockerfile
    ports:
      - "8080:80"
    environment:
      - DATABASE_URL=postgres://db/myapp
    depends_on:
      - db
    networks:
      - app-network
    volumes:
      - ./app:/app
    restart: unless-stopped

  db:
    image: postgres:16-alpine
    environment:
      - POSTGRES_DB=myapp
      - POSTGRES_PASSWORD_FILE=/run/secrets/db_password
    volumes:
      - db-data:/var/lib/postgresql/data
    networks:
      - app-network
    secrets:
      - db_password

networks:
  app-network:
    driver: bridge

volumes:
  db-data:

secrets:
  db_password:
    file: ./secrets/db_password.txt
```

### Key Points
- Use specific image tags (not `latest`)
- Use `.env` files for environment variables
- Define health checks
- Use secrets for sensitive data
- Specify restart policies
- Define resource limits
- Use named volumes for persistence

### Environment Variables

**Using .env file:**
```bash
# .env
DATABASE_URL=postgres://localhost/myapp
SECRET_KEY=mysecret
```

**In compose file:**
```yaml
services:
  web:
    env_file:
      - .env
    # Or specific variables
    environment:
      - DATABASE_URL=${DATABASE_URL}
```

### Profiles

**Define profiles:**
```yaml
services:
  web:
    # Always runs

  debug:
    profiles: ["debug"]
    # Only runs with --profile debug
```

**Use profiles:**
```bash
docker-compose --profile debug up -d
docker compose --profile debug up -d
podman-compose --profile debug up -d
```

## Podman-Specific Considerations

### Podman Compose vs Docker Compose

**Podman Compose:**
- Python-based implementation
- Works with Podman daemon or rootless
- May have slight syntax differences

**Generate Kubernetes YAML from Compose:**
```bash
# Podman can generate K8s YAML
podman-compose up
podman generate kube mypod > pod.yaml
```

### Systemd Integration

**Generate systemd unit from compose:**
```bash
# Start services
podman-compose up -d

# Get pod name
podman pod ps

# Generate systemd unit
podman generate systemd --name mypod_pod --files

# Move to systemd directory
mv *.service ~/.config/systemd/user/

# Enable
systemctl --user enable --now mypod_pod.service
```

## Common Issues

**Issue**: Compose file version not supported
- Use version 3.8 or omit version (modern compose)
- Check compose tool version

**Issue**: Services can't communicate
- Ensure services are on same network
- Use service names as hostnames

**Issue**: Port already in use
- Check for conflicting services: `lsof -i :8080`
- Change port mapping in compose file

**Issue**: Volumes not persisting
- Use named volumes instead of anonymous
- Check volume definitions

**Issue**: Environment variables not loading
- Verify .env file location (same directory as compose file)
- Check for quotes: `KEY=value` not `KEY="value"`

## Output Format

After compose operation:
```
✓ Services started successfully
  Runtime: docker-compose / podman-compose
  Services: 3 running
    - web (0.0.0.0:8080->80/tcp)
    - db (postgres:16-alpine)
    - redis (redis:7-alpine)

Next steps:
  - View logs: docker-compose logs -f
  - Check status: docker-compose ps
  - Stop services: docker-compose down
```

## Advanced Features

### Override Files

**Use override for development:**
```bash
# docker-compose.override.yml automatically loaded
docker-compose up -d

# Or specify explicitly
docker-compose -f docker-compose.yml -f docker-compose.dev.yml up -d
```

### Health Checks

```yaml
services:
  web:
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost/health"]
      interval: 30s
      timeout: 10s
      retries: 3
      start_period: 40s
```

### Resource Limits

```yaml
services:
  web:
    deploy:
      resources:
        limits:
          cpus: '0.5'
          memory: 512M
        reservations:
          cpus: '0.25'
          memory: 256M
```
