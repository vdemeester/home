source ../common/builder-functions.sh

nixpkgs_builder_setup
ls $stdenv

BINARIES="ocla kcl kdd kdp kselect kubectl-get-secret"

copy_binaries_to_bin "$src/kubernetes" $BINARIES

install_shell_completions "$src/kubernetes" \
    kcl _kcl \
    kdd _kdd \
    kdp _kdp \
    kselect _kselect \
    ocla _ocla
