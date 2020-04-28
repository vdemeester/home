#!/usr/bin/env bash
# This scripts tries to automate the NixOS installation
# as much as possible.
# curl https://gitlab.com/vdemeester/home/-/raw/master/install.sh | sh

set -euo pipefail

read -p "Hostname: " name
read -p "Disk:" disk
read -p "Swap size (8GiB)": swap
swap=${swap:-8GiB}

echo "Partiton disk"
set -x

parted ${disk} -- mklabel gpt
parted ${disk} -- mkpart primary 512MiB -${swap}
parted ${disk} -- mkpart primary linux-swap -${swap} 100%
parted ${disk} -- mkpart ESP fat32 1MiB 512MiB
parted ${disk} -- set 3 boot on

set +x
echo "Format partiion"
set -x

mkfs.ext4 -L nixos ${disk}1
mkswap -L swap ${disk}2
mkfs.fat -F 32 -n boot ${disk}3

set +x
echo "Mount filesystems"
set -x

mount /dev/disk/by-label/nixos /mnt
mkdir -p /mnt/boot
mount /dev/disk/by-label/boot /mnt/boot
swapon ${disk}2

set +x
echo "Setup configuration"
set -x

mkdir -p /mnt/etc
git clone https://gitlab.com/vdemeester/home.git /mnt/etc/nixos
echo -n ${name} > /mnt/etc/nixos/hostname

set +x
echo "Run the following:"
echo "- nixos-generate-config --root /mnt"
echo "- (once ready) nixos-install"
