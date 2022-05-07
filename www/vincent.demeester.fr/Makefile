# /Make/ it happen
# :PROPERTIES:
# :header-args: :tangle /etc/nixos/www/vincent.demeester.fr/Makefile
# :header-args+: :comments org
# :ID:       6191455e-95bc-4abc-a5f5-a62606ab2ea7
# :END:
#
# In order to publish this website, I am using [[file:make.org][make]]. In a nutshell, I am going to define a
# few target to get the content from my notes, export org files into html and copy more or
# less everything to the =public= folder. I will also define a clean and a publish target.
#
# The first part of my =Makefile= will be to define some constants that I want to use later
# on. Those are mainly to easily change where to look for the notes or where the emacs
# configuration is.


EMACS =
ifndef EMACS
EMACS = "emacs"
endif

DOTEMACS =
ifndef DOTEMACS
DOTEMACS = "~/.config/emacs"
endif

PUBLISH =
ifndef PUBLISH
PUBLISH = vincent.demeester.fr
endif

NOTES = ~/desktop/org/notes



# The default target will be name =build=.


all: build

# Building =public/= and publishing it
# :PROPERTIES:
# :ID:       853f3d6b-f385-4091-9f9e-b04d17794e5c
# :END:
#
# To build the website, we will be using [[file:emacs.org][Emacs]] in batch mode, with some shared library *and*
# the actual [[id:631ced7a-f3f7-4a77-81a8-4a1884a6c4d4][publish]] script.


.PHONY: build
build: lib/publish.el lib/publish-common.el build-articles
	@echo "Publishing... with current Emacs configurations."
	${EMACS} --debug-init --batch --directory $(DOTEMACS)/lisp/ --directory $(DOTEMACS)/lisp/vorg/ \
		--load lib/publish-common.el --load lib/publish.el \
		--funcall publish

.PHONY: build-articles
build-articles: $(NOTES)
	rsync -arv --delete --copy-links --exclude='*.private.org' --exclude='*.db' $(NOTES)/ content/articles/

$(NOTES):
	$(error $(NOTES) doesn't exists…)



# The =publish= target is gonna be really simple: I just need to copy the content to
# =~/desktop/sites= on the current machine, and the rest is automated.


#rsync -a --progress --copy-links --delete public/assets/.fancyindex/ ~/desktop/sites/dl.sbr.pm/.fancyindex/
#rsync -a --progress --copy-links --delete public/ ~/desktop/sites/${PUBLISH}/
.PHONY: publish
publish: build
	rsync -ave ssh --progress --copy-links --delete public/assets/.fancyindex/ kerkouane.vpn:/var/www/dl.sbr.pm/.fancyindex/
	rsync -ave ssh --progress --copy-links --delete public/ kerkouane.vpn:/var/www/${PUBLISH}/

# Local server
# :PROPERTIES:
# :ID:       0e26a52e-5f66-42c6-934d-b45cfc9745b2
# :END:
#
# Let's use =miniserve= (using [[file:nix.org][Nix]] with =nix-shell=) to serve the static website locally to
# validate my changes.


.PHONY: serve
serve:
	nix-shell -p miniserve --command "miniserve --port=8181 --index=index.html public/"

# Final nits of the =Makefile=
# :PROPERTIES:
# :ID:       5bec22c8-491b-4e03-855a-4b5f859473cf
# :END:
#
# One of the final step is to install the git hooks if any. I tend to have this target in all my
# personal =Makefile= at least. Let's also define a =pre-commit= target that will hold
# anything we need to do at =pre-commit=.


.PHONY: install-hooks
install-hooks:
	if [ -e .git ]; then nix-shell -p git --run 'git config core.hooksPath .githooks'; fi

.PHONY: pre-commit
pre-commit: README.md



# And the final target is the =clean= one. This will remove any compile emacs-lisp file
# (=*.elc=), the =public= folder, and some org-mode metadata.


.PHONY: clean
clean:
	@echo "Cleaning up.."
	@-rm -rvf *.elc
	@-rm -rvf public
	#@-rm -rv ~/.org-timestamps/*
