#!/usr/bin/env bash
# univ: update niv (and generate a nice commit)

# TODO: Maybe rewrite this in Python..

# TODO libguestfs-with-appliance
# TODO create images with qemu-img and virt-format --format=qcow2 --filesystem=ext4 -a vdisk1.qcow2
# TODO Create xml by hand instead of virt-install

set -euo pipefail

# export QEMU_URI=qemu+ssh://vincent@wakasu.home/system
# virt-install --connect=${QEMU_URI} \
#              --name="ocp4-bootstrap" --vcpus=4 --ram=8192 \
#              --disk path=/var/lib/libvirt/images/ocp-bootstrap.qcow2,bus=virtio,size=120 \
#              --boot menu=on --print-xml > ocp4-bootstrap.xml
# virsh --connect=${QEMU_URI} \
    #       define --file ocp4-bootstrap.xml

HOST=${HOST:-wakasu.home}
QEMU_URI="qemu+ssh://${HOST}/system"
RSYNC_COMMAND="rsync -avzHXShPse ssh --progress"
VIRSH_COMMAND="virsh --connect=${QEMU_URI}"
NODES=(
    k8sn1
    k8sn2
    k8sn3
)

build() {
    for n in ${NODES[@]}; do
        logs=$(mktemp)
        output=$(mktemp)
        echo "Build ${n} node (logs: ${logs})…"
        nixos-generate -f qcow -c ./systems/hosts/${n}.nix 2>${logs} 1>${output}
        echo "Syncthing image to ${HOST}…"
        ${RSYNC_COMMAND} $(cat ${output} | tr -d '\n') root@${HOST}:/var/lib/libvirt/images/${n}.qcow2
    done
}

delete() {
    for n in ${NODES[@]}; do
        echo "Delete ${n} node…"
        ${VIRSH_COMMAND} list | grep ${n} && {
            ${VIRSH_COMMAND} destroy ${n}
        } || {
            echo "skipping, not present…"
        }
        ${VIRSH_COMMAND} undefine ${n} --remove-all-storage || echo "Failed to erase.. might not exists"
    done
}

# Bootstrap the cluster, assuming images are built and synced
bootstrap() {
    echo "Bootstrap k8s cluster on ${HOST}"
    k8sn1_mac="52:54:00:dd:a3:30"
    k8sn2_mac="52:54:00:dd:a3:31"
    k8sn3_mac="52:54:00:dd:a3:32"
    folder=$(mktemp -d)
    for n in ${NODES[@]}; do
        mac_addr=${n}_mac
        virt-install --connect=${QEMU_URI} \
                     --name="${n}" --vcpus=4 --ram=8192 \
                     --network bridge=br1,mac.address=${!mac_addr} \
                     --disk path=/var/lib/libvirt/images/${n}.qcow2,bus=virtio,size=10 \
                     --print-xml > ${folder}/${n}.xml
                     # --disk path=/var/lib/libvirt/images/${n}-data.qcow2,bus=virtio,size=40 \
        echo "Node ${n} : ${folder}/${n}.xml"
        ${VIRSH_COMMAND} define --file ${folder}/${n}.xml
    done
}

status() {
    echo "TBD: display the status of the cluster"
}

main() {
    set +u
    ARG=$1
    set -u
    case ${ARG} in
        "build")
            build
            ;;
        "delete")
            delete
            ;;
        "bootstrap")
            bootstrap
            ;;
        "status")
            status
            ;;
        *)
            echo "No such subcommand"
            exit 1
            ;;
    esac
}

main $@
