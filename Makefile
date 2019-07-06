all: build

.PHONY: update
update:
	nix-channel --update

.PHONY: assets
assets:
	cp -Rv ~/sync/nixos/machines.nix assets/

.PHONY: build
build: assets
	home-manager build

.PHONY: switch
switch: assets
	home-manager switch
.PHONY: clean
clean:
	unlink result
