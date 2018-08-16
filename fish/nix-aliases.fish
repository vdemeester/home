function mr
    nix run nixpkgs.mr -c mr $argv
end

function wakeonlan
    nix run nixpkgs.python36Packages.wakeonlan -c wakeonlan $argv
end

function pprof
    nix run -f ~/.config/nixpkgs/channels.nix unstable.pprof -c pprof $argv
end

function golangci-lint
    nix run -f ~/.config/nixpkgs/channels.nix unstable.golangci-lint -c golangci-lint $argv
end
