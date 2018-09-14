function _def_git_nix_run_aliases
    set -l packages git-annex:gitAndTools.git-annex grv:gitAndTools.grv
    for s in $packages
	_nix_run_package $s nixpkgs
    end
end

_def_git_nix_run_aliases
