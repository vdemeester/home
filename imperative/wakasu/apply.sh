#!/usr/bin/env bash

# Wakasu Post-Install Setup Script
# Description: Post-installation commands for Fedora setup

set -euo pipefail

# Color output for better readability
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly NC='\033[0m' # No Color

# Logging functions
log_info() {
	echo -e "${GREEN}[INFO]${NC} $*"
}

log_warn() {
	echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
	echo -e "${RED}[ERROR]${NC} $*" >&2
}

setup.syncthing() {
	log_info "Install syncthing..."
	sudo dnf install -y syncthing

	log_info "Enable syncthing user service..."
	systemctl --user enable syncthing.service
	systemctl --user start syncthing.service
}

setup.wireguard() {
	log_info "Install wireguard..."
	sudo dnf install -y wireguard-tools

	log_info "Setup wireguard configuration..."
	if [ -z "${WG_PRIVATE_KEY:-}" ]; then
		log_warn "WG_PRIVATE_KEY not set, skipping wireguard configuration"
		log_warn "Set WG_PRIVATE_KEY environment variable and re-run to configure"
		return 0
	fi

	sudo tee /etc/wireguard/wg0.conf <<EOF
[Interface]
PrivateKey = ${WG_PRIVATE_KEY}
## Client IP
Address = 10.100.0.90/24

## if you have DNS server running
# DNS = 192.168.11.1

[Peer]
PublicKey = +H3fxErP9HoFUrPgU19ra9+GDLQw+VwvLWx3lMct7QI=

## to pass internet trafic 0.0.0.0 but for peer connection only use 192.168.11.0/24, or you can also specify comma separated IPs
AllowedIPs =  10.100.0.0/24

Endpoint = 167.99.17.238:51820
PersistentKeepalive = 25
EOF

	log_info "Wireguard configuration created at /etc/wireguard/wg0.conf"
}

setup.default_packages() {
	log_info "Install default packages..."
	sudo dnf install -y helix acpi
}

# Main setup function
main() {
	log_info "Starting Wakasu post-install setup..."

	setup.default_packages
	setup.syncthing
	setup.wireguard

	log_info "Post-install setup completed successfully!"
}

# Run main function
main "$@"
