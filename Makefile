# Makefile for home

EMACS =
ifndef EMACS
EMACS = "emacs"
endif

DOTEMACS =
ifndef DOTEMACS
DOTEMACS = "~/.config/emacs"
endif

PUBLISH_FOLDER =
ifndef PUBLISH_FOLDER
PUBLISH_FOLDER=~/desktop/sites/beta.sbr.pm
endif

.PHONY: all
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

.PHONY: clean-www
clean-www:
	@rm -rvf *.elc
	@rm -rv ~/.org-timestamps/*

.PHONY: build-www
build-www: ${HOME}/src/www/publish-common.el publish.el
	@echo "Publishing... with current Emacs configurations."
	${EMACS} --batch --directory $(DOTEMACS)/lisp/ \
		--load ${HOME}/src/www/publish-common.el --load publish.el \
		--funcall org-publish-all

${HOME}/src/www/Makefile: ${HOME}/src/www/
${HOME}/src/www/publish-common.el: ${HOME}/src/www/

${HOME}/src/www/:
	test -d ${HOME}/src/www || git clone git@git.sr.ht:~vdemeester/www.git ${HOME}/src/www/
