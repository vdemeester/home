{ ... }: {
  imports = [
    ../hardware-configuration.nix
    ../cloud/digitalocean/networking.nix # generated at runtime by nixos-infect
    ../configuration/users.nix
    ../configuration/custom-packages.nix
  ];

  boot.cleanTmpDir = true;
  networking.firewall.allowPing = true;
  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCqaMAW+tsun4gRn/XZZ3evoEDlYOPKx+7h1O/PBzwbiJzrR+5XpP32p1n1krsbF1jHyxTK+3GO9BXfFq2ag/RoO6u6jbXJwzJX8+ZYlcRfhkPu/ixDkF0ADMTrtzw+OYiYU9uYnqY5bj2En2uOfZCIOtsvE4yHCgeXTR9Xo+owB5ci3d5lfF1URTg3dJNzQykBnM06Fu8fYth/5DBInEau4h5N8XrkWUU0K/zYdWl0ws6dAHZYo+JZoqmF+o/ptb00e9cegZHEtLfa/IXC/GwD4gcLrnIpETr+HQBHHJH5PXPuEnO73rDaRBVWHWAErkj1/3OoH/m71pyvc4rLPZs0N4B9vdCpblGj/IWivLLnQaZQFTNRPWe45WaqVpIR1VS5UpHElr0cjqroaqXI0RcBVQ8v6fMytITP1B9u2s0G07ScLMJLlWwA5GhySWULskzcYNqCLmbSnmEzsfFRzawDCHn0BWiwRdnMVo1HwbUwvAw7z2my5b83fauf45jrwuAElCSUBvibEeHHHmY84FeiJhAUIpeu9yXNpGnc1kSOibJQjZwlRH54bzMuiq5UvaaV+kH0gfNEQOvxHJBdf4r9gxb+JG4/VnurdgTLulBiGXDuDBvhOvCb3bPRpswZbm67EPgG5HMJtjeEdaLG/yV0mhZ7Jl6rk8dvh6IrXKkKew== vincent@honshu.local"
  ];
	time.timeZone = "Europe/Paris";

	system = {
		stateVersion = "16.09";	
		autoUpgrade = {
			enable = true;
			dates = "13:00";
		};
	};

}
