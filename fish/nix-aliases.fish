function mr
    nix run nixpkgs.mr -c mr $argv
end

function wakeonlan
    nix run nixpkgs.python36Packages.wakeonlan -c wakeonlan $argv
end

function op
    nix run -f ~/.config/nixpkgs/channels.nix unstable.1password -c op $argv
end

function update-desktop-database
    nix run nixpkgs.desktop-file-utils -c update-desktop-database $argv
end

function sshfs
    nix run nixpkgs.sshfs -c sshfs $argv
end

function lspci
    nix run nixpkgs.pciutils -c lspci $argv
end

function lsusb
    nix run nixpkgs.usbutils -c lsusb $argv
end
