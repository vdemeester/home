all: build

.PHONY: update
update:
	nix-channel --update

.PHONY: pull
pull:
	(cd overlays/emacs-overlay && git pull --rebase)

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

.PHONY: publish
publish:
	cp *.org ~/desktop/org/technical/configurations/
