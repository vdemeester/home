# NetworkManage Workflow

Create, manage, and troubleshoot container networks.

## Process

1. **Detect Runtime**
   - Run `DetectRuntime.sh` to determine Docker/Podman

2. **Determine Network Operation**
   - List networks
   - Create network
   - Connect/disconnect containers
   - Inspect network
   - Remove network

3. **Execute Network Command**
   - Run appropriate network command
   - Configure network settings

4. **Verify Configuration**
   - Check network connectivity
   - Verify container connections

## List Networks

**Show all networks:**
```bash
docker network ls
podman network ls
```

**Filter networks:**
```bash
# By driver
docker network ls --filter driver=bridge
podman network ls --filter driver=bridge

# By name
docker network ls --filter name=myapp
podman network ls --filter name=myapp
```

## Create Networks

### Bridge Network (Default)

**Create simple bridge network:**
```bash
docker network create mynetwork
podman network create mynetwork
```

**Create with subnet:**
```bash
docker network create \
  --subnet=172.20.0.0/16 \
  --gateway=172.20.0.1 \
  mynetwork

podman network create \
  --subnet=172.20.0.0/16 \
  --gateway=172.20.0.1 \
  mynetwork
```

**Create with IP range:**
```bash
docker network create \
  --subnet=172.20.0.0/16 \
  --ip-range=172.20.240.0/20 \
  --gateway=172.20.0.1 \
  mynetwork
```

**Create with custom driver options:**
```bash
docker network create \
  --driver bridge \
  --opt "com.docker.network.bridge.name=br-myapp" \
  --opt "com.docker.network.bridge.enable_ip_masquerade=true" \
  mynetwork
```

### Host Network

**Use host network (container shares host network stack):**
```bash
docker run --network host myapp
podman run --network host myapp
```

Note: Cannot create host network, it's built-in.

### Overlay Network (Swarm/Multi-host)

**Create overlay network:**
```bash
# Docker Swarm only
docker network create --driver overlay myoverlay

# With encryption
docker network create \
  --driver overlay \
  --opt encrypted \
  myoverlay
```

### Macvlan Network

**Create macvlan network:**
```bash
docker network create \
  --driver macvlan \
  --subnet=192.168.1.0/24 \
  --gateway=192.168.1.1 \
  -o parent=eth0 \
  mymacvlan
```

### None Network (No networking)

**Run with no network:**
```bash
docker run --network none myapp
podman run --network none myapp
```

## Connect Containers

### Connect to Network

**Connect running container:**
```bash
docker network connect mynetwork container_name
podman network connect mynetwork container_name
```

**Connect with specific IP:**
```bash
docker network connect --ip 172.20.0.100 mynetwork container_name
podman network connect --ip 172.20.0.100 mynetwork container_name
```

**Connect with alias:**
```bash
docker network connect --alias db mynetwork container_name
podman network connect --alias db mynetwork container_name
```

### Disconnect from Network

**Disconnect container:**
```bash
docker network disconnect mynetwork container_name
podman network disconnect mynetwork container_name
```

**Force disconnect:**
```bash
docker network disconnect -f mynetwork container_name
podman network disconnect -f mynetwork container_name
```

### Run Container on Network

**Start container on specific network:**
```bash
docker run --network mynetwork --name web nginx
podman run --network mynetwork --name web nginx
```

**With custom IP:**
```bash
docker run --network mynetwork --ip 172.20.0.100 --name web nginx
podman run --network mynetwork --ip 172.20.0.100 --name web nginx
```

**With network alias:**
```bash
docker run --network mynetwork --network-alias webapp --name web nginx
podman run --network mynetwork --network-alias webapp --name web nginx
```

## Inspect Networks

**View network details:**
```bash
docker network inspect mynetwork
podman network inspect mynetwork
```

**Get specific field:**
```bash
# List connected containers
docker network inspect -f '{{range .Containers}}{{.Name}} {{end}}' mynetwork

# Get subnet
docker network inspect -f '{{range .IPAM.Config}}{{.Subnet}}{{end}}' mynetwork

# Get gateway
docker network inspect -f '{{range .IPAM.Config}}{{.Gateway}}{{end}}' mynetwork
```

**JSON output with jq:**
```bash
docker network inspect mynetwork | jq '.[0].IPAM.Config'
podman network inspect mynetwork | jq '.[0].subnets'
```

## Remove Networks

**Remove network:**
```bash
docker network rm mynetwork
podman network rm mynetwork
```

**Remove multiple networks:**
```bash
docker network rm network1 network2 network3
podman network rm network1 network2 network3
```

**Remove all unused networks:**
```bash
docker network prune
podman network prune
```

**Force remove (with confirmation):**
```bash
docker network prune -f
podman network prune -f
```

## Network Drivers

### Bridge (Default)

**Best for:**
- Single-host deployments
- Isolated container groups
- Development environments

**Characteristics:**
- Software bridge on host
- NAT for external access
- DNS resolution between containers
- Isolated from host network

### Host

**Best for:**
- Maximum network performance
- Direct port binding
- Legacy applications

**Characteristics:**
- No network isolation
- Container uses host network stack
- No port mapping needed
- Higher performance, lower security

### Overlay

**Best for:**
- Multi-host deployments
- Docker Swarm
- Microservices across hosts

**Characteristics:**
- Spans multiple Docker hosts
- Requires swarm mode or key-value store
- Encrypted overlay traffic option

### Macvlan

**Best for:**
- Legacy applications
- Direct Layer 2 access
- Container needs MAC address

**Characteristics:**
- Containers get own MAC address
- Appear as physical devices
- Direct connection to physical network

### None

**Best for:**
- Maximum isolation
- Custom networking
- Security-critical containers

**Characteristics:**
- No network interface
- Complete network isolation
- Must manually configure if needed

## DNS and Service Discovery

### Automatic DNS

**Containers on same user-defined network can resolve each other by name:**
```bash
# Create network
docker network create myapp

# Start containers
docker run -d --network myapp --name web nginx
docker run -d --network myapp --name db postgres

# web can reach db by name
docker exec web ping db
docker exec web curl http://db:5432
```

### Custom DNS

**Set DNS servers:**
```bash
docker run --dns 8.8.8.8 --dns 8.8.4.4 myapp
podman run --dns 8.8.8.8 myapp
```

**Set DNS search domain:**
```bash
docker run --dns-search example.com myapp
podman run --dns-search example.com myapp
```

### Network Aliases

**Add multiple aliases:**
```bash
docker network connect --alias db --alias database mynetwork container_name
```

## Port Mapping

**Map ports:**
```bash
# Map container port to host
docker run -p 8080:80 nginx  # host:container
podman run -p 8080:80 nginx

# Map to specific interface
docker run -p 127.0.0.1:8080:80 nginx
podman run -p 127.0.0.1:8080:80 nginx

# Map range
docker run -p 8080-8090:8080-8090 myapp
podman run -p 8080-8090:8080-8090 myapp

# Map UDP
docker run -p 53:53/udp dns-server
podman run -p 53:53/udp dns-server
```

**Publish all exposed ports:**
```bash
docker run -P nginx  # Maps to random host ports
podman run -P nginx
```

**Check port mappings:**
```bash
docker port container_name
podman port container_name
```

## Network Troubleshooting

### Check Container Network

**Get container IP:**
```bash
docker inspect -f '{{range.NetworkSettings.Networks}}{{.IPAddress}}{{end}}' container_name
podman inspect -f '{{.NetworkSettings.IPAddress}}' container_name
```

**List all containers on network:**
```bash
docker network inspect mynetwork -f '{{range .Containers}}{{.Name}} {{.IPv4Address}}{{"\n"}}{{end}}'
```

### Test Connectivity

**From container:**
```bash
# Ping another container
docker exec web ping db

# Test port connectivity
docker exec web nc -zv db 5432

# DNS resolution
docker exec web nslookup db

# Check routes
docker exec web ip route
```

**From host:**
```bash
# Ping container
ping 172.20.0.100

# Test port
nc -zv 172.20.0.100 80
curl http://172.20.0.100
```

### Debug Network Issues

**Check network exists:**
```bash
docker network ls | grep mynetwork
```

**Check container is connected:**
```bash
docker network inspect mynetwork | grep container_name
```

**Check firewall rules:**
```bash
# Docker creates iptables rules
sudo iptables -L DOCKER
sudo iptables -L DOCKER-USER
sudo iptables -t nat -L DOCKER
```

**Check bridge:**
```bash
# List bridges
ip link show type bridge

# Show bridge details
bridge link show
```

## Advanced Networking

### Custom Bridge

**Create custom bridge on host:**
```bash
# Create bridge
sudo ip link add br-custom type bridge
sudo ip addr add 172.30.0.1/24 dev br-custom
sudo ip link set br-custom up

# Use with Docker
docker network create \
  --driver bridge \
  --opt "com.docker.network.bridge.name=br-custom" \
  mycustom
```

### Network Namespaces

**Enter container network namespace:**
```bash
# Get container PID
PID=$(docker inspect -f '{{.State.Pid}}' container_name)

# Enter network namespace
sudo nsenter -t $PID -n ip addr show
```

### IPv6 Support

**Enable IPv6:**
```bash
# Docker daemon config (/etc/docker/daemon.json)
{
  "ipv6": true,
  "fixed-cidr-v6": "2001:db8:1::/64"
}

# Create IPv6 network
docker network create --ipv6 --subnet=2001:db8:1::/64 mynetwork
```

## Podman-Specific Networking

### CNI Plugins

**Podman uses CNI (Container Network Interface):**
```bash
# CNI config location
/etc/cni/net.d/

# List available plugins
ls /usr/libexec/cni/
```

### Rootless Networking

**Rootless uses slirp4netns by default:**
```bash
# Check rootless network
podman network inspect podman

# Create rootless network
podman network create mynetwork
```

### Pod Networking

**Containers in a pod share network:**
```bash
# Create pod
podman pod create --name mypod -p 8080:80

# Add containers (share network)
podman run -d --pod mypod --name web nginx
podman run -d --pod mypod --name app myapp

# They can communicate via localhost
```

## Best Practices

### Security

- Use user-defined networks, not default bridge
- Isolate sensitive services
- Limit external exposure
- Use internal networks for backend services

### Organization

- Name networks descriptively (e.g., `frontend`, `backend`, `data`)
- One network per application tier
- Document network topology
- Use labels for organization

### Performance

- Use host networking for high-throughput apps (with caution)
- Keep containers on same network for inter-container communication
- Use overlay networks sparingly (overhead)

## Common Issues

**Issue**: Containers can't communicate
- Ensure containers are on the same network
- Check firewall rules
- Verify DNS resolution

**Issue**: Port already in use
- Check host port availability: `lsof -i :8080`
- Use different host port
- Stop conflicting service

**Issue**: Network already exists
- Choose different name
- Remove old network: `docker network rm`

**Issue**: Can't remove network
- **Cause**: Containers still connected
- **Solution**: Disconnect or remove containers first

## Output Format

After network operation:
```
✓ Network created successfully
  Runtime: docker/podman
  Name: myapp-network
  Driver: bridge
  Subnet: 172.20.0.0/16
  Gateway: 172.20.0.1

  Connected containers: 0

  Connect container:
    docker network connect myapp-network container_name

  Run container on network:
    docker run --network myapp-network myapp
```

## Quick Reference

```bash
# Create network
docker network create mynetwork

# List networks
docker network ls

# Inspect network
docker network inspect mynetwork

# Connect container
docker network connect mynetwork container_name

# Disconnect container
docker network disconnect mynetwork container_name

# Remove network
docker network rm mynetwork

# Clean up unused networks
docker network prune
```
