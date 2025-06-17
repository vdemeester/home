((magit-commit "--signoff")
 (magit-fetch "--prune")
 (magit-submodule "--recursive" "--rebase" "--remote")
 (magit-tag "--sign"))
