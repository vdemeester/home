# Makefile for home
HOSTS          = $(shell nix flake show --json --all-systems 2>/dev/null | yq '.nixosConfigurations.[] | key')
HOSTS_BUILD    = $(addprefix host/, $(addsuffix /build,$(HOSTS)))

.PHONY: all
all: dry-build

# Host operations
.PHONY: hosts
hosts: ${HOSTS_BUILD}
	echo ${HOSTS_BUILD} ${HOSTS}

host/%/build: FORCE
	nix build .#nixosConfigurations.$*.config.system.build.toplevel --no-link

host/%/boot: FORCE
	nixos-rebuild --target-host root@$*.sbr.pm --flake .#$* boot

host/%/switch: FORCE
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

# Local system operations
.PHONY: boot
boot:
	sudo nixos-rebuild --flake .# boot

.PHONY: switch
switch:
	sudo nixos-rebuild --flake .# switch

.PHONY: dry-build
dry-build:
	nixos-rebuild --flake .# dry-build

.PHONY: build
build:
	nixos-rebuild --flake .# build

# Development
.PHONY: pre-commit
pre-commit: fmt

.PHONY: fmt
fmt:
	nixfmt-plus

# Dotfiles
.PHONY: dots
dots:
	@$(MAKE) -C dots

# Maintenance
.PHONY: clean
clean: clean-system clean-results

.PHONY: clean-system
clean-system:
	sudo nix-env --profile /nix/var/nix/profiles/system --delete-generations 15d

.PHONY: clean-results
clean-results:
	rm -f result result-*

# Update flake inputs
.PHONY: update
update:
	nix flake update

FORCE:
