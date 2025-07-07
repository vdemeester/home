#!/usr/bin/env bash
set -eufo pipefail

cPWD="$(dirname $(readlink -f $0))"
source ${cPWD}/lib/functions.sh

function build() {
	docker build --target output_collector --output type=local,dest="firmwares" -f keyboards/eyelash_corne.Dockerfile keyboards/eyelash_corne
}

function flash() {
	side=${1}
	id=${2}

	echo -n "$(echo_red ) Connect ${side} side as usb storage: "
	while [[ ! -e /dev/disk/by-id/${id} ]]; do
		echo -n "$(echo_blue .)"
		sleep 1
	done
	echo " ✅"
	sudo umount /mnt 2>/dev/null >/dev/null || true
	sudo mount /dev/disk/by-id/${id} /mnt
	sudo cp -bv firmwares/eyelash_corne_${side}.uf2 /mnt/CURRENT.UF2
	sync
	sudo umount /mnt
}

build

# flash left usb-Adafruit_nRF_UF2_6863BEB9EE8668EC-0:0
echo "$(echo_green ) Left side is completed.."
sleep 2

flash right usb-Adafruit_nRF_UF2_0E9CC2AD6581EE32-0:0
echo $(echo_green ) "Right side is completed.. enjoy 🥳"
