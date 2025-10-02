source $stdenv/setup
ls $stdenv

BINARIES="aicommit git-commit-suggest-label"

mkdir -p $out/bin
for b in ${BINARIES}; do
	cp $src/ai/${b} $out/bin/
done

installShellCompletion --cmd aicommit --zsh $src/ai/_aicommit
