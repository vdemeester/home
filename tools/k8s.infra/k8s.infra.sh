#!/usr/bin/env bash
# univ: update niv (and generate a nice commit)

set -euo pipefail

HOST=${HOST:-wakasu.home}
RSYNC_COMMAND="rsync -avzHXShPse ssh --progress"

main() {
    for n in k8sn1 k8sn2 k8sn3; do
        logs=$(mktemp)
        output=$(mktemp)
        echo "Build ${n} node (logs: ${logs})…"
        nixos-generate -f qcow -c ./systems/hosts/${n}.nix 2>${logs} 1>${output}
        echo "Syncthing image to ${HOST}…"
        ${RSYNC_COMMAND} $(cat ${output} | tr -d '\n') root@${HOST}:/var/lib/libvirt/images/${n}.qcow2 --dry-run
    done
}

main $@
