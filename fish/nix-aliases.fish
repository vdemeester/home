function _def_nix_run_aliases
    set -l stable mr sshfs ncdu wakeonlan:python36Packages.wakeonlan lspci:pciutils lsusb:usbutils beet:beets gotop virt-manager:virtmanager pandoc
    set -l unstable op:_1password update-desktop-database:desktop-file-utils lgogdownloader
    for s in $stable
	_nix_run_package $s nixpkgs
    end
    for s in $unstable
	_nix_run_package $s unstable ~/.config/nixpkgs/channels.nix
    end
end

_def_nix_run_aliases
