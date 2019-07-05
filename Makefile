all: dry-build

.PHONY: assets
assets:
	mkdir -p assets
	cp -Rv /home/vincent/sync/nixos/* assets/

.PHONY: dry-build
dry-build: assets
	nixos-rebuild dry-build

.PHONY: switch
switch: assets
	nixos-rebuild switch

.PHONY: clean
clean:
	nix-env --profile /nix/var/nix/profiles/system --delete-generations 15d
