# Home configuration files

This repository is using `home-manager` and `nix`.

- clone this repository in `$HOME/.config/nixpkgs`
- run `nix-shell https://github.com/rycee/home-manager/archive/master.tar.gz -A install` (maybe twice :D)
- run `echo -n "Passphrase: "; read -i PASSPHRASE; env PASSPHRASE=$PASSPHRASE home-manager switch;`