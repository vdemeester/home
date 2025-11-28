# fedora-vm

Create and provision Fedora VMs with libvirt and cloud-init.

## Overview

`fedora-vm` is a tool that automates the creation of Fedora-based virtual machines using libvirt/virt-install. It uses cloud-init for automatic provisioning and can execute scripts inside the VM once it's ready.

## Features

- **Automated provisioning**: Uses cloud-init to automatically configure the VM
- **SSH key injection**: Automatically injects your SSH public key
- **Script execution**: Run scripts or commands inside the VM
- **Flexible configuration**: Customize memory, CPUs, disk size, and more
- **Cleanup options**: Optionally destroy the VM after script execution

## Requirements

- libvirt and virt-install
- qemu
- genisoimage or mkisofs (for creating cloud-init ISO)
- SSH access configured

## Usage

### Basic Usage

Create a VM and run a script:

```bash
fedora-vm --name test-vm --script ./setup.sh
```

Create a VM and run a command:

```bash
fedora-vm --name test-vm --command "dnf install -y httpd && systemctl start httpd"
```

### Advanced Usage

Create a VM with custom resources:

```bash
fedora-vm \
  --name big-vm \
  --memory 4096 \
  --cpus 4 \
  --disk-size 20 \
  --script ./build.sh
```

Create a temporary VM that gets cleaned up after execution:

```bash
fedora-vm \
  --name temp-vm \
  --script ./test.sh \
  --cleanup
```

Keep the VM running after script execution:

```bash
fedora-vm \
  --name dev-vm \
  --script ./setup.sh \
  --keep
```

## Options

```
-n, --name NAME           VM name (required)
-s, --script PATH         Path to script to execute in VM
-c, --command "CMD"       Command string to execute in VM
-m, --memory MB           Memory in MB (default: 2048)
-p, --cpus NUM            Number of CPUs (default: 2)
-d, --disk-size GB        Disk size in GB (default: 10)
-k, --ssh-key PATH        SSH public key to inject (default: ~/.ssh/id_rsa.pub)
-u, --user USERNAME       VM username (default: fedora)
-v, --fedora-version VER  Fedora version (default: 41)
--cleanup                 Delete VM after script execution
--keep                    Keep VM running after script execution
--workdir PATH            Working directory for temp files
-h, --help                Show help message
```

## Environment Variables

- `FEDORA_VERSION`: Fedora version to use (default: 41)
- `LIBVIRT_URI`: Libvirt connection URI (default: qemu:///system)
- `VM_USER`: Default username (default: fedora)
- `SSH_KEY`: Default SSH public key path

## Examples

### Test a package installation

```bash
cat > test-install.sh <<'EOF'
#!/bin/bash
set -euo pipefail

# Update system
sudo dnf update -y

# Install packages
sudo dnf install -y nginx postgresql

# Verify installations
nginx -v
postgres --version
EOF

fedora-vm --name test-install --script test-install.sh --cleanup
```

### Build and test software

```bash
cat > build-test.sh <<'EOF'
#!/bin/bash
set -euo pipefail

# Install build dependencies
sudo dnf install -y gcc make automake

# Clone and build project
git clone https://github.com/user/project.git
cd project
make
make test
EOF

fedora-vm --name build-test --memory 4096 --script build-test.sh --cleanup
```

### Setup development environment

```bash
fedora-vm \
  --name dev-env \
  --memory 4096 \
  --cpus 4 \
  --disk-size 30 \
  --command "sudo dnf install -y vim git golang nodejs" \
  --keep
```

## How It Works

1. **Download**: Downloads the Fedora cloud image if not already present
2. **Configure**: Creates cloud-init configuration with SSH keys
3. **Create**: Uses virt-install to create the VM
4. **Wait**: Waits for the VM to get an IP and for cloud-init to complete
5. **Execute**: Runs your script or command via SSH
6. **Cleanup**: Optionally destroys the VM when done

## Cloud-init Configuration

The tool automatically creates a cloud-init configuration that:

- Creates a user with sudo access (default: `fedora`)
- Injects your SSH public key
- Disables password authentication
- Updates package lists
- Installs basic utilities (vim, curl, wget, git)
- Enables SSH access

## Notes

- The VM is created using the default libvirt network
- The first run will download the Fedora cloud image (~200-400MB)
- Subsequent runs will reuse the downloaded image
- SSH host key checking is disabled for convenience
- By default, VMs are destroyed after script execution unless `--keep` is used
