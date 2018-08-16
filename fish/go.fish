set -gx GOPATH $HOME

function pprof
    nix run -f ~/.config/nixpkgs/channels.nix unstable.pprof -c pprof $argv
end

function golangci-lint
    nix run -f ~/.config/nixpkgs/channels.nix unstable.golangci-lint -c golangci-lint $argv
end
