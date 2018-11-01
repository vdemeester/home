function _def_git_nix_run_aliases
    set -l packages git-annex:gitAndTools.git-annex grv:gitAndTools.grv
    set -l unstable git-appraise:gitAndTools.git-appraise
    for s in $packages
	_nix_run_package $s nixpkgs
    end
    for s in $unstable
	_nix_run_package $s unstable ~/.config/nixpkgs/channels.nix
    end
end

_def_git_nix_run_aliases
