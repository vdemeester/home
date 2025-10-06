#! /usr/bin/env nix-shell
#! nix-shell -i bash -p qmk

set -eufo pipefail
TARGET_USER=vincent

checkout() {
	if [[ ! -d ./build/qmk_firmware ]]; then
		git clone https://github.com/qmk/qmk_firmware ./build/qmk_firmware
	fi
}

update() {
	git submodule update --init --recursive
	for i in qmk_firmware keychron_firmware; do
		(
			[[ -e ./build/${i} ]] && {
				cd ./build/${i}
				qmk git-submodule
			} || true
		)
	done
}

symlink() {
	local p=qmk_firmware
	local keyboard="zsa/moonlander"
	rm -f build/${p}/keyboards/${keyboard}/keymaps/${TARGET_USER}
	mkdir -p build/${p}/keyboards/${keyboard}/keymaps
	ln -rvsf ${PWD}/config build/${p}/keyboards/${keyboard}/keymaps/${TARGET_USER}
	# [[ -e build/${p}/users/common ]] || ln -rvsf ${PWD}/common build/${p}/users/vincent
}

action() {
	local keyboard="zsa/moonlander"
	local action=$1
	local p=qmk_firmware
	symlink ${keyboard}
	make BUILD_DIR=${PWD}/build -j1 -C build/${p} ${keyboard}:${TARGET_USER}:${action}
}

build() {
	action build
}

flash() {
	action flash
}

clean() {
	rm -rf build
	git submodule update -f --recursive
}

help() {
	echo "build.sh [update/clean/build/flash]"
}

[[ -z ${1-""} ]] && {
	set +x
	echo "need at least one arg"
	help
	exit 1
}

if [[ $1 == update ]]; then
	update
	exit
elif [[ $1 == checkout ]]; then
	checkout ${2:-""}
	exit
elif [[ ${1} == clean ]]; then
	clean
	exit
fi

case "$1" in
build) build ;;
flash) flash ;;
*)
	echo "Wrong argument $1"
	help
	exit 1
	;;
esac
