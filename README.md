# Home configuration files

This repository is using `home-manager` and `nix`.

If you don't have `nix` installed, run `curl https://nixos.org/nix/install | sh` to install it.

- clone this repository in `$HOME/.config/nixpkgs`
- create a `home.nix` file with the content you want.
  Some machines files already exists that you can use :
  ```
  { pkgs, ... }:

  {
	imports = [
	    ./machines/hokkaido.nix
  	];
  }
  ```
- run `nix-shell https://github.com/rycee/home-manager/archive/master.tar.gz -A install` (maybe twice :D)
- run `echo -n "Passphrase: "; read -i PASSPHRASE; env PASSPHRASE=$PASSPHRASE home-manager switch;`
