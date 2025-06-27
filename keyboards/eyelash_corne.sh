#!/usr/bin/env bash
# shellcheck disable=SC1091
set -eufo pipefail

cPWD="$(dirname $(readlink -f $0))"
# TODO: we'll want to checkout those (ideally, using nix)
# zmkRepo="$(readlink -f $cPWD/zmk)"
eyeZMK="$(readlink -f $cPWD/new_corne)"
# cd $zmkRepo/app || exit 1
cd $eyeZMK/zephyr || exit 1
# TODO: Need to run west init (and west update, and west zephyr-export ?) if we didn't do it yet
# source $zmkRepo/.venv/bin/activate
source ${cPWD}/lib/functions.sh

function flash() {
	side=${1}
	id=${2}

	west build -b eyelash_corne_${side} \
		--build-dir build-${side} \
		-S studio-rpc-usb-uart -- \
		-DSHIELD=nice_view \
		-DCONFIG_ZMK_STUDIO=y \
		-DCONFIG_ZMK_STUDIO_LOCKING=n \
		-DZMK_CONFIG=$cPWD/keyboards/eyelash_corne \
		-DKEYMAP_FILE=$cPWD/keyboards/eyelash_corne/eyelash_corne.keymap #\
	# -DZMK_EXTRA_MODULES=$eyeZMK
}

flash left usb-Adafruit_nRF_UF2_D9D14D5F56CF8D6F-0:0
echo "$(echo_green ) Left side is completed.."
sleep 2

flash right usb-Adafruit_nRF_UF2_07E2C44920A78BC8-0:0
echo $(echo_green ) "Right side is completed.. enjoy 🥳"
