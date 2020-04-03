# Makefile for home

.PHONY: all build build-www clean publish pull switch update

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

all: build

update:
	nix-channel --update

pull:
	(cd overlays/emacs-overlay && git pull --rebase)

assets:
	cp -Rv ~/sync/nixos/machines.nix assets/

build: assets
	home-manager build

switch: assets
	home-manager switch

clean:
	unlink result

clean-www:
	@rm -rvf *.elc
	@rm -rv ~/.org-timestamps/*

build-www: ${HOME}/src/www/publish-common.el publish.el
	@echo "Publishing... with current Emacs configurations."
	${EMACS} --batch --directory $(DOTEMACS)/lisp/ \
		--load ${HOME}/src/www/publish-common.el --load publish.el \
		--funcall org-publish-all

${HOME}/src/www/Makefile: ${HOME}/src/www/
${HOME}/src/www/publish-common.el: ${HOME}/src/www/

${HOME}/src/www/:
	test -d ${HOME}/src/www || git clone git@git.sr.ht:~vdemeester/www.git ${HOME}/src/www/
