#!/usr/bin/env bash
set -eufo pipefail

cPWD="$(dirname "$(readlink -f "$0")")"
# shellcheck disable=SC1091
source "${cPWD}/lib/functions.sh"

function build() {
	tar -cf - Dockerfile build.yaml config zephyr | docker build --target output_collector --output type=local,dest="firmwares" -f Dockerfile -
}

function flash() {
	side=${1}
	id=${2}

	echo -n "$(echo_red) Connect ${side} side as usb storage: "
	while [[ ! -e /dev/disk/by-id/"${id}" ]]; do
		echo -n "$(echo_blue .)"
		sleep 1
	done
	echo " ✅"

	if [[ -d /run/media/"${USER}"/NICENANO ]]; then
		echo "$(echo_green ) Using auto-mounted /run/media/${USER}/NICENANO"
		cp -bv "firmwares/eyelash_corne_${side}.uf2" /run/media/"${USER}"/NICENANO/CURRENT.UF2
	else
		sudo umount /mnt 2>/dev/null >/dev/null || true
		sudo mount /dev/disk/by-id/"${id}" /mnt
		sudo cp -bv "firmwares/eyelash_corne_${side}.uf2" /mnt/CURRENT.UF2
	fi
}

help() {
	echo "go.sh [build/flash]"
}

[[ -z ${1-""} ]] && {
	set +x
	echo "need at least one arg"
	help
	exit 1
}

case "$1" in
build) build ;;
flash)
	build

	flash left usb-Adafruit_nRF_UF2_6863BEB9EE8668EC-0:0
	echo "$(echo_green ) Left side is completed.."
	sleep 2

	flash right usb-Adafruit_nRF_UF2_0E9CC2AD6581EE32-0:0
	echo "$(echo_green )" "Right side is completed.. enjoy 🥳"
	;;
*)
	echo "Wrong argument $1"
	help
	exit 1
	;;
esac
