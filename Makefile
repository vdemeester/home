# Makefile for home

# Variables
EMACS =
ifndef EMACS
EMACS = "emacs"
endif

DOTEMACS = ~/.config/emacs
DOTNIXPKGS = ~/.config/nixpkgs
SYNCDIR = ~/sync/nixos
PUBLISH_FOLDER = ~/desktop/sites/beta.sbr.pm

# Targets
.PHONY: all
all: build

.PHONY: update
update:
	nix-channel --update

.PHONY: pull
pull:
	(cd overlays/emacs-overlay && git pull --rebase)

# home-manager setup
.PHONY: assets
assets:
	mkdir -p assets
	cp -Rv ~/sync/nixos/machines.nix assets/

.PHONY: build
build: assets
	@if test $(USER) = root;\
	then\
		nixos-rebuild build;\
	else\
		home-manager build;\
	fi

.PHONY: switch
switch: assets
	@if test $(USER) = root;\
	then\
		nixos-rebuild build;\
	else\
		home-manager build;\
	fi

# Cleaning
.PHONY: clean
clean:
	unlink result

.PHONY: clean-www
clean-www:
	@rm -rvf *.elc
	@rm -rv ~/.org-timestamps/*

# Documentatino build and publishing
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

# Setup and doctor
.PHONY: doctor
doctor:
	@echo "Validate the environment"
	@readlink $(DOTEMACS) || $(error $(DOTEMACS) is not correctly linked, you may need to run setup)
	@readlink $(DOTNIXPKGS) || $(error $(DOTNIXPKGS) is not correctly linked, you may need to run setup)

.PHONY: setup
setup: $(DOTEMACS) $(DOTNIXPKGS) $(SYNCDIR)
	@echo $(USER)

$(DOTEMACS):
	@echo "Link $(DOTEMACS) to $(CURDIR)/tools/emacs"
	@ln -s $(CURDIR)/tools/emacs $(DOTEMACS)

$(DOTNIXPKGS):
	@echo "Link $(DOTNIXPKGS) to $(CURDIR)"
	@ln -s $(CURDIR) $(DOTNIXPKGS)

$(SYNCDIR):
	$(error $(SYNCDIR) is not present, you need to configure syncthing before running this command)
