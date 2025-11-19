#!/usr/bin/env bash

# Nagoya Post-Install Setup Script
# Description: Post-installation commands for Debian setup

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

# TODO: config.txt (diff with default, nvme, ...)

setup.syncthing() {
	log_info "Install syncthing..."
	sudo apt update && sudo apt install gnupg2 curl apt-transport-https -y
	curl -fsSL https://syncthing.net/release-key.txt |
		sudo gpg --dearmor -o /etc/apt/trusted.gpg.d/syncthing.gpg
	echo "deb https://apt.syncthing.net/ syncthing release" |
		sudo tee /etc/apt/sources.list.d/syncthing.list
	sudo apt update && sudo apt install synching
	# FIXME: setup user system service
}

setup.docker() {
	log_info "Install docker..."
	# cleanup any old package
	for pkg in docker.io docker-doc docker-compose podman-docker containerd runc; do sudo apt remove -y $pkg; done
	sudo apt update
	sudo apt install -y ca-certificates curl
	sudo install -m 0755 -d /etc/apt/keyrings
	sudo curl -fsSL https://download.docker.com/linux/debian/gpg -o /etc/apt/keyrings/docker.asc
	sudo chmod a+r /etc/apt/keyrings/docker.asc

	# Add the repository to Apt sources:
	# shellcheck disable=SC1091
	sudo tee /etc/apt/sources.list.d/docker.sources <<EOF
Types: deb
URIs: https://download.docker.com/linux/debian
Suites: $(. /etc/os-release && echo "$VERSION_CODENAME")
Components: stable
Signed-By: /etc/apt/keyrings/docker.asc
EOF

	sudo apt update
	sudo apt install docker-ce docker-ce-cli containerd.io docker-buildx-plugin
}

setup.kind() {
	log_info "Install kind..."
	[ "$(uname -m)" = aarch64 ] && curl -Lo ./kind https://kind.sigs.k8s.io/dl/v0.30.0/kind-linux-arm64
	chmod +x ./kind
	sudo mv ./kind /usr/local/bin/kind
}

setup.wireguard() {
	log_info "Setup wireguard..."
	sudo tee /etc/wireguard/wg0.conf <<EOF
[Interface]
PrivateKey = ${WG_PRIVATE_KEY}
## Client IP
Address = 10.100.0.80/24

## if you have DNS server running
# DNS = 192.168.11.1

[Peer]
PublicKey = +H3fxErP9HoFUrPgU19ra9+GDLQw+VwvLWx3lMct7QI=
 
## to pass internet trafic 0.0.0.0 but for peer connection only use 192.168.11.0/24, or you can also specify comma separated IPs
AllowedIPs =  10.100.0.0/24

Endpoint = 167.99.17.238:51820
PersistentKeepalive = 25
EOF
	return 0
}

# Main setup function
main() {
	log_info "Starting Nagoya post-install setup..."

	setup.wireguard
	setup.docker
	setup.kind

	setup.syncthing
	# Add your commands here

	log_info "Post-install setup completed successfully!"
}

# Run main function
main "$@"
