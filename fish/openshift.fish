function _def_openshift_nix_run_aliases
    set -l packages openshift oc:openshift
    for s in $packages
	_nix_run_package $s nixpkgs
	# _nix_run_package $s unstable ~/.config/nixpkgs/channels.nix
    end
end

_def_openshift_nix_run_aliases
