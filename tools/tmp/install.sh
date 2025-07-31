#! /usr/bin/env nix-shell
#! nix-shell -i bash -p git
# LEGACY: This script is deprecated and should be replaced with the modern
# disko-based installation script at /install.sh
# This scripts tries to automate the NixOS installation
# as much as possible.
# curl https://gitlab.com/vdemeester/home/-/raw/master/install.sh | sh

set -eo pipefail

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
sleep 20

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
git clone --recurse-submodules https://gitlab.com/vdemeester/home.git /mnt/etc/nixos
echo -n ${name} > /mnt/etc/nixos/hostname
nixos-generate-config --root /mnt

set +x
echo "Run the following:"
echo "- populate assets/ folder"
echo "- do last minutes changes"
echo "Once you are done, just exit the shell (C-D or exit)"
bash || true

echo "Run nixos-install"
set -x
nixos-install --channel $(nix eval --raw '(import /mnt/etc/nixos/nix/sources.nix).nixos.outPath') --no-channel-copy
