function mr
    nix run nixpkgs.mr -c mr $argv
end

function wakeonlan
    nix run nixpkgs.python36Packages.wakeonlan -c wakeonlan $argv
end

function op
    nix run -f ~/.config/nixpkgs/channels.nix unstable.1password -c op $argv
end
