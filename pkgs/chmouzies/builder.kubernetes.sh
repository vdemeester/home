source $stdenv/setup
ls $stdenv

BINARIES="ocla kcl kdd kdp kselect kubectl-get-secret"

mkdir -p $out/bin
for b in ${BINARIES}; do
	cp $src/kubernetes/${b} $out/bin/
done

installShellCompletion --cmd kcl --zsh $src/kubernetes/_kcl
installShellCompletion --cmd kdd --zsh $src/kubernetes/_kdd
installShellCompletion --cmd kdp --zsh $src/kubernetes/_kdp
installShellCompletion --cmd kselect --zsh $src/kubernetes/_kselect
installShellCompletion --cmd ocla --zsh $src/kubernetes/_ocla
