function git-annex
    nix run nixpkgs.gitAndTools.git-annex -c git-annex $argv
end
