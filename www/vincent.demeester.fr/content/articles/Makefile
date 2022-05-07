all: tangle

org_files = $(patsubst %.org, %.org.tangle, $(wildcard *.org)))

tangle: $(org_files)

%.org.tangle: %.org 
	emacs --batch --eval "(progn (require 'ob-tangle) (org-babel-tangle-file \"$<\"))"
