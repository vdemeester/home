set -gx GOPATH $HOME

function _def_go_nix_run_aliases
    set -l unstable pprof golangci-lint
    for s in $unstable
	_nix_run_package $s unstable ~/.config/nixpkgs/channels.nix
    end
end

_def_go_nix_run_aliases
