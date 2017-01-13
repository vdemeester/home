{ configs, pkgs, ...}:

{
	environment.systemPackages = with pkgs; [
		calibre
		libreoffice
		gimp
		haskellPackages.hledger
		haskellPackages.hledger-ui
		haskellPackages.hledger-web
	];
}
