{ config, pkgs, ... }:

{
	programs = {
		 zsh.enable = true;
	};
	environment = {
		variables = {
			EDITOR = pkgs.lib.mkOverride 0 "vim";
		};
		systemPackages = with pkgs; [
				file
				git
				htop
				iotop
				lsof
				netcat
				psmisc
				tmux
				tree
				vim
				wget
				zsh
		];
	};
	i18n = {
		consoleFont = "Lat2-Terminus16";
		consoleKeyMap = "fr";
		defaultLocale = "en_US.UTF-8";
		# supportedLocales = [ "en_US/ISO-8859-1" "fr_FR/ISO-8859-1" "fr_FR@euro/ISO-8859-15" ];
	};
	nix = {
		useSandbox = true;
		gc = {
			automatic = true;
			dates = "00:30";
			options = "--delete-older-than 30d";
		};
		# if hydra is down, don't wait forever
		extraOptions = ''
		    gc-keep-output = true
		    connect-timeout = 20
		'';
	};
	nixpkgs = {
		config = {
			allowUnfree = true;
		};
	};

	system = {
		stateVersion = "17.03";
	#	autoUpgrade = {
	#		enable = true;
	#		dates = "13:00";
	#	};
	};
	systemd.services.nixos-update = {
		description = "NixOS Upgrade";
		unitConfig.X-StopOnRemoval = false;
		serviceConfig.Type = "oneshot";
	
		environment = config.nix.envVars //
		{ inherit (config.environment.sessionVariables) NIX_PATH;
			HOME = "/root";
		};
		path = [ pkgs.gnutar pkgs.xz pkgs.git config.nix.package.out ];
		script = ''
			cd /etc/nixos/
			git pull --autostash --rebase
			nix-channel --update nixos
		'';
		#startAt = "12:00";
		startAt = "weekly";
	};
}
