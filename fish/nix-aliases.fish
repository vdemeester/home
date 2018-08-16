function mr
    nix run nixpkgs.mr -c mr $argv
end

function wakeonlan
    nix run nixpkgs.python36Packages.wakeonlan -c wakeonlan $argv
end

function pprof
    nix run nixpkgs.pprof -c pprof $argv
end
