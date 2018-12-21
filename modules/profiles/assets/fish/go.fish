set -gx GOPATH $HOME
set -gx GOPROXY http://go.cache.home:3000

function _def_go_nix_run_aliases
    set -l unstable pprof
    for s in $unstable
        _nix_run_package $s unstable ~/.config/nixpkgs/channels.nix
    end
end

_def_go_nix_run_aliases
