function _def_nix_run_aliases
    set -l stable mr sshfs wakeonlan:python36Packages.wakeonlan lspci:pciutils lsusb:usbutils beet:beets
    set -l unstable op:1password update-desktop-database:destkop-file-utils
    for s in $stable
	_nix_run_package $s
    end
    for s in $unstable
	_nix_run_package $s "-f ~/.config/nixpkgs/channels.nix"
    end
end

function _nix_run_package
    set -l s $argv[1]
    set -l package (string split ":" $s)
    switch (count $package)
	case 1
	    _nix_run $s $s $argv[2]
	case 2
	    _nix_run $package[1] $package[2] $argv[2]
    end
end

function _nix_run
    set -l c $argv[1]
    set -l p $argv[2]
    set -l extra $argv[3]
    function $c --inherit-variable c --inherit-variable p --inherit-variable extra
	nix run $extra "nixpkgs.$p" -c $c $argv
    end
end

_def_nix_run_aliases
