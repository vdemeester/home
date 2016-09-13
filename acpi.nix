{ config, pkgs, ... };

{
	environment.systemPackages = with pkgs; [
		acpi
	];

	services = {
		acpid = {
			enable = true;
		};
};
}
