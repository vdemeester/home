source $stdenv/setup
ls $stdenv

BINARIES="gh-prcheck gh-actionfollow gh-clone gh-issuef gh-issuecreate gh-completer git-branch-cleanup git-remote-branch git-wt-delete git-wt-create"

mkdir -p $out/bin
for b in ${BINARIES}; do
	cp $src/git/${b} $out/bin/
done

installShellCompletion --cmd gh-issuecreate --zsh $src/git/_gh-issuecreate
installShellCompletion --cmd gh-clone --zsh $src/git/_gh-clone
