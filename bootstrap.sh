#!/usr/bin/env bash
# This scripts aims to detect which system is running, and bootstrap
# the home configuration accordingly. So far the current setup are
# supported:
# - NixOS (>= 19.03 more or less)
# - Fedora (>= 30)
# - Mac OS X (>= 10.14)

set -e

setup_nixos() {
    echo "NixOS detected"
}

setup_fedora() {
    echo "Fedora detected"
}
		
setup_osx() {
    echo "Mac OS X detected"
    if [[ "$kernel_name" == "Darwin" ]]; then
        IFS=$'\n' read -d "" -ra sw_vers < <(awk -F'<|>' '/key|string/ {print $3}' \
                            "/System/Library/CoreServices/SystemVersion.plist")
        for ((i=0;i<${#sw_vers[@]};i+=2)) {
		case ${sw_vers[i]} in
                    ProductName)          darwin_name=${sw_vers[i+1]} ;;
                    ProductVersion)       osx_version=${sw_vers[i+1]} ;;
                    ProductBuildVersion)  osx_build=${sw_vers[i+1]}   ;;
		esac
            }
     fi
}

IFS=" " read -ra uname <<< "$(uname -srm)"
kernel_name="${uname[0]}"
kernel_version="${uname[1]}"
kernel_machine="${uname[2]}"

case "$kernel_name" in
    "Linux" | "GNU")
	if [[ -f "/etc/os-release" || -f "/usr/lib/os-release" ]]; then
                files=("/etc/os-release" "/usr/lib/os-release")

                # Source the os-release file
                for file in "${files[@]}"; do
                    source "$file" && break
                done
		case "$ID" in
		    "nixos")
			setup_nixos ;;
		    "fedora")
			setup_fedora ;;
		esac
	fi ;;
    "Darwin")
	setup_osx ;;
esac
