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

setup.nix() {
	log_info "Setting up Nix package manager with SELinux support..."

	# Check if Nix is already installed
	if command -v nix &>/dev/null; then
		log_info "Nix is already installed, skipping installation"
		return 0
	fi

	# Install required dependencies
	log_info "Installing dependencies..."
	sudo dnf install -y policycoreutils-python-utils

	# Configure SELinux contexts for Nix
	log_info "Configuring SELinux contexts..."
	sudo semanage fcontext --add --type etc_t '/nix/store/[^/]+/etc(/.*)?'
	sudo semanage fcontext --add --type lib_t '/nix/store/[^/]+/lib(/.*)?'
	sudo semanage fcontext --add --type systemd_unit_file_t '/nix/store/[^/]+/lib/systemd/system(/.*)?'
	sudo semanage fcontext --add --type man_t '/nix/store/[^/]+/man(/.*)?'
	sudo semanage fcontext --add --type bin_t '/nix/store/[^/]+/s?bin(/.*)?'
	sudo semanage fcontext --add --type usr_t '/nix/store/[^/]+/share(/.*)?'
	sudo semanage fcontext --add --type var_run_t '/nix/var/nix/daemon-socket(/.*)?'
	sudo semanage fcontext --add --type usr_t '/nix/var/nix/profiles(/per-user/[^/]+)?/[^/]+'

	# Create Nix directories
	log_info "Creating Nix directories..."
	sudo mkdir -p /nix

	# Install Nix with daemon support
	log_info "Installing Nix..."
	sh <(curl -L https://nixos.org/nix/install) --daemon

	# Source Nix profile
	if [ -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]; then
		# shellcheck source=/dev/null
		. /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
		log_info "Nix installed successfully! Please restart your shell or run: source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"
	else
		log_warn "Nix profile script not found, you may need to configure it manually"
	fi
}

setup.selinux_policies() {
	log_info "Configuring SELinux policies for system-manager..."

	# Check if SELinux is enabled
	if ! command -v getenforce &>/dev/null || [ "$(getenforce)" = "Disabled" ]; then
		log_info "SELinux is not enabled, skipping SELinux policy configuration"
		return 0
	fi

	# Install policycoreutils if not already installed
	sudo dnf install -y policycoreutils-python-utils

	# Configure SELinux context for systemd files managed by system-manager
	log_info "Setting SELinux contexts for system-manager..."

	# Allow systemd to read symbolic links created by system-manager
	if [ -d /etc/systemd/system ]; then
		sudo restorecon -R /etc/systemd/system || true
	fi

	log_info "SELinux policies configured successfully"
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

	log_info "Setup wireguard private key..."
	if [ -z "${WG_PRIVATE_KEY:-}" ]; then
		log_warn "WG_PRIVATE_KEY not set, skipping wireguard configuration"
		log_warn "Set WG_PRIVATE_KEY environment variable and re-run to configure"
		return 0
	fi

	# Create wireguard directory if it doesn't exist
	sudo mkdir -p /etc/wireguard

	# Write the private key to the expected location for the wireguard-client module
	echo "${WG_PRIVATE_KEY}" | sudo tee /etc/wireguard/private.key > /dev/null
	sudo chmod 600 /etc/wireguard/private.key

	log_info "Wireguard private key created at /etc/wireguard/private.key"
	log_info "The rest of the WireGuard configuration is managed by system-manager"
}

setup.default_packages() {
	log_info "Install default packages..."
	sudo dnf install -y helix acpi
}

setup.system_manager() {
	log_info "Activating system-manager configuration..."

	# Check if Nix is installed
	if ! command -v nix &>/dev/null; then
		log_warn "Nix is not installed, skipping system-manager activation"
		log_warn "Run this script again after Nix is installed and you've restarted your shell"
		return 0
	fi

	# Get the path to this script to locate the repository
	local script_dir
	script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
	local repo_root
	repo_root="$(cd "${script_dir}/../.." && pwd)"

	log_info "Repository root: ${repo_root}"

	# Check if we're in the right repository
	if [ ! -f "${repo_root}/flake.nix" ]; then
		log_error "Cannot find flake.nix in repository root: ${repo_root}"
		log_error "Please ensure this script is in the correct location"
		return 1
	fi

	# Activate system-manager configuration
	log_info "Building and activating wakasu system-manager configuration..."
	if nix run 'github:numtide/system-manager' -- switch --flake "${repo_root}#wakasu"; then
		log_info "System-manager configuration activated successfully!"
	else
		log_error "Failed to activate system-manager configuration"
		log_warn "You can manually activate it later with:"
		log_warn "  nix run 'github:numtide/system-manager' -- switch --flake ${repo_root}#wakasu"
		return 1
	fi
}

# Main setup function
main() {
	log_info "Starting Wakasu post-install setup..."

	setup.default_packages
	setup.nix
	setup.selinux_policies

	# Note: syncthing and wireguard will be managed by system-manager
	# These functions set up the initial configuration files
	setup.syncthing
	setup.wireguard

	# Activate system-manager configuration to manage services
	setup.system_manager

	log_info "Post-install setup completed successfully!"
	log_info ""
	log_info "Next steps:"
	log_info "  1. If Nix was just installed, restart your shell: source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"
	log_info "  2. Configure Syncthing via the web interface"
	log_info "  3. Set up WireGuard private key if not already done"
	log_info "  4. Run 'systemctl status syncthing wireguard-wg0' to check service status (if using system-manager)"
}

# Run main function
main "$@"
