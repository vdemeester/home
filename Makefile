# Makefile for home
HOSTS          = $(shell nix flake show --json --all-systems 2>/dev/null | yq '.nixosConfigurations.[] | key')
HOSTS_BUILD    = $(addprefix host/, $(addsuffix /build,$(HOSTS)))

.PHONY: all
all: help

.PHONY: help
help: ## Show this help message
	@echo 'Usage: make [target]'
	@echo ''
	@echo 'Available targets:'
	@awk 'BEGIN {FS = ":.*##"} \
		/^[a-zA-Z_\/%-]+:.*?##/ { printf "  %-30s %s\n", $$1, $$2 } \
		/^##@/ { printf "\n%s\n", substr($$0, 5) }' $(MAKEFILE_LIST)

##@ Host Operations (Remote)

.PHONY: hosts
hosts: ${HOSTS_BUILD} ## List and build all hosts
	echo ${HOSTS_BUILD} ${HOSTS}

host/%/build: FORCE ## Build a specific remote host (e.g., make host/demeter/build)
	nix build .#nixosConfigurations.$*.config.system.build.toplevel --no-link

host/%/boot: FORCE ## Deploy to remote host and activate on next boot (e.g., make host/demeter/boot)
	nixos-rebuild --target-host root@$*.sbr.pm --flake .#$* boot

host/%/switch: FORCE ## Deploy to remote host and activate immediately (e.g., make host/demeter/switch)
	nixos-rebuild --target-host root@$*.sbr.pm --flake .#$* switch

# Host-specific overrides (non-standard DNS/network)
.PHONY: host/nagoya/boot
host/nagoya/boot:
	nixos-rebuild --target-host root@192.168.1.80 --flake .#nagoya boot

.PHONY: host/kobe/boot
host/kobe/boot:
	nixos-rebuild --target-host root@192.168.1.77 --flake .#kobe boot

.PHONY: host/aix/boot
host/aix/boot:
	nixos-rebuild --target-host root@10.100.0.89 --flake .#aix boot

.PHONY: host/kerkouane/boot
host/kerkouane/boot:
	nixos-rebuild --target-host root@kerkouane.vpn --flake .#kerkouane boot

.PHONY: host/kerkouane/switch
host/kerkouane/switch:
	nixos-rebuild --target-host root@kerkouane.vpn --flake .#kerkouane switch

##@ Local System Operations

.PHONY: boot
boot: ## Build and activate local system on next boot
	sudo nixos-rebuild --flake .# boot

.PHONY: switch
switch: ## Build and activate local system immediately
	sudo nixos-rebuild --flake .# switch

.PHONY: dry-build
dry-build: ## Test build local system without activating
	nixos-rebuild --flake .# dry-build

.PHONY: build
build: ## Build local system without activating
	nixos-rebuild --flake .# build

##@ Development

.PHONY: pre-commit
pre-commit: fmt ## Run pre-commit checks (formatting)

.PHONY: fmt
fmt: ## Format Nix files
	nixfmt-plus

##@ Dotfiles

.PHONY: dots
dots: ## Build dotfiles
	@$(MAKE) -C dots

##@ Keyboards

.PHONY: keyboards keyboards/moonlander/build keyboards/moonlander/flash keyboards/moonlander/update keyboards/moonlander/clean
.PHONY: keyboards/eyelash_corne/build keyboards/eyelash_corne/flash
.PHONY: keyboards/draw keyboards/moonlander/draw keyboards/eyelash_corne/draw

keyboards: ## Show keyboard-specific help
	@$(MAKE) -C keyboards help

keyboards/moonlander/build: ## Build Moonlander QMK firmware
	@$(MAKE) -C keyboards moonlander/build

keyboards/moonlander/flash: ## Flash Moonlander firmware
	@$(MAKE) -C keyboards moonlander/flash

keyboards/moonlander/update: ## Update Moonlander QMK repository
	@$(MAKE) -C keyboards moonlander/update

keyboards/moonlander/clean: ## Clean Moonlander build artifacts
	@$(MAKE) -C keyboards moonlander/clean

keyboards/eyelash_corne/build: ## Build Eyelash Corne ZMK firmware
	@$(MAKE) -C keyboards eyelash_corne/build

keyboards/eyelash_corne/flash: ## Flash Eyelash Corne firmware
	@$(MAKE) -C keyboards eyelash_corne/flash

keyboards/draw: ## Generate keymap SVGs for all keyboards
	@$(MAKE) -C keyboards draw

keyboards/moonlander/draw: ## Generate Moonlander keymap SVG
	@$(MAKE) -C keyboards moonlander/draw

keyboards/eyelash_corne/draw: ## Generate Eyelash Corne keymap SVG
	@$(MAKE) -C keyboards eyelash_corne/draw

##@ DNS Management

.PHONY: dns-show
dns-show: ## Show current DNS records
	@bash tools/show-dns.sh

.PHONY: dns-update-gandi
dns-update-gandi: ## Update Gandi DNS records
	@bash tools/update-gandi-dns.sh

.PHONY: dns-update-gandi-dry-run
dns-update-gandi-dry-run: ## Dry-run Gandi DNS update
	@bash tools/update-gandi-dns.sh --dry-run

##@ Maintenance

.PHONY: clean
clean: clean-system clean-results ## Clean old generations and build results

.PHONY: clean-system
clean-system: ## Delete system generations older than 15 days
	sudo nix-env --profile /nix/var/nix/profiles/system --delete-generations 15d

.PHONY: clean-results
clean-results: ## Remove build result symlinks
	rm -f result result-*

.PHONY: update
update: ## Update flake inputs
	nix flake update

FORCE:
