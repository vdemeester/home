{ config, lib, pkgs, ... }:

let
  inherit (lib) importTOML attrsets hasAttr optionals versionAtLeast mkIf;
  metadata = importTOML ../../ops/hosts.toml;
  hasSSHAttr = name: value: hasAttr "ssh" value;
  authorizedKeys = attrsets.mapAttrsToList
    (name: value: value.ssh.pubkey)
    (attrsets.filterAttrs hasSSHAttr metadata.hosts);

  hasConfigVirtualizationContainers = builtins.hasAttr "containers" config.virtualisation;
  isContainersEnabled = if hasConfigVirtualizationContainers then config.virtualisation.containers.enable else false;
in
{
  warnings = if (versionAtLeast config.system.nixos.release "21.11") then [ ] else [ "NixOS release: ${config.system.nixos.release}" ];
  users.users.vincent = {
    createHome = true;
    uid = 1000;
    description = "Vincent Demeester";
    extraGroups = [ "wheel" "input" ]
      ++ optionals config.networking.networkmanager.enable [ "networkmanager" ]
      ++ optionals config.modules.desktop.enable [ "audio" "video" ]
      # ++ optionals config.profiles.scanning.enable [ "lp" "scanner" ]
      ++ optionals config.networking.networkmanager.enable [ "networkmanager" ]
      ++ optionals config.virtualisation.docker.enable [ "docker" ]
      ++ optionals config.virtualisation.buildkitd.enable [ "buildkit" ]
      ++ optionals config.modules.virtualisation.libvirt.enable [ "libvirtd" ]
      ++ optionals config.services.nginx.enable [ "nginx" ]
      ++ optionals config.security.tpm2.enable [ "tss" ];
    shell = mkIf config.programs.zsh.enable pkgs.zsh;
    isNormalUser = true;
    openssh.authorizedKeys.keys = authorizedKeys
      ++ metadata.ssh.keys.vincent
      ++ metadata.ssh.keys.root;
    initialPassword = "changeMe";
    subUidRanges = [{ startUid = 100000; count = 65536; }];
    subGidRanges = [{ startGid = 100000; count = 65536; }];
  };

  nix = {
    settings = {
      trusted-users = [ "vincent" ];
    };
    sshServe.keys = authorizedKeys;
  };

  security = {
    pam = {
      # Nix will hit the stack limit when using `nixFlakes`.
      loginLimits = [
        { domain = config.users.users.vincent.name; item = "stack"; type = "-"; value = "unlimited"; }
      ];
    };
  };

  # Enable user units to persist after sessions end.
  system.activationScripts.loginctl-enable-linger-vincent = lib.stringAfter [ "users" ] ''
    ${pkgs.systemd}/bin/loginctl enable-linger ${config.users.users.vincent.name}
  '';

  # To use nixos config in home-manager configuration, use the nixosConfig attr.
  # This make it possible to import the whole configuration, and let each module
  # load their own.
  # FIXME(vdemeester) using nixosConfig, we can get the NixOS configuration from
  # the home-manager configuration. This should help play around the conditions
  # inside each "home-manager" modules instead of here.
  home-manager.users.vincent = lib.mkMerge
    (
      [
        (import ./core)
        (import ./mails { hostname = config.networking.hostName; pkgs = pkgs; })
      ]
      ++ optionals config.modules.editors.emacs.enable [
        (import ./dev/emacs.nix)
      ]
      ++ optionals config.modules.dev.enable [
        (import ./dev)
        # TODO Move it elsewhere ? 
        (import ./containers/kubernetes.nix)
        (import ./containers/openshift.nix)
        (import ./containers/tekton.nix)
        {
          # Enable only on dev, could do something better than this longterm 😀
          services.keybase.enable = true;
        }
      ]
      ++ optionals config.modules.dev.containers.enable [
        (import ./containers)
      ]
      ++ optionals config.modules.desktop.enable [ (import ./desktop) ]
      ++ optionals (config.networking.hostName == "wakasu" || config.networking.hostName == "aomi") [
        {
          # Move this to its own module
          home.packages = with pkgs; [
            libosinfo
            asciinema
            oathToolkit
            p7zip
          ];
          home.file."bin/msmtp" = {
            text = ''
              #!${pkgs.stdenv.shell}
              ${pkgs.libnotify}/bin/notify-send "Sending mail ✉️"
              ${pkgs.msmtp}/bin/msmtp --read-envelope-from $@
            '';
            executable = true;
          };
          programs.mbsync.enable = true;
          # programs.lieer.enable = true;
          programs.aerc.enable = true;
          programs.msmtp.enable = true;
          programs.mu.enable = true;
          # programs.notmuch.enable = true;
          accounts.email = {
            maildirBasePath = "desktop/mails";
            accounts = {
              "icloud" = {
                primary = true;
                address = "vincent@demeester.fr";
                userName = "vdemeester@icloud.com";
                realName = "Vincent Demeester";
                passwordCommand = "${pkgs.passage}/bin/passage show mails/icloud/vdemeester";
                imap.host = "imap.mail.me.com";
                smtp.host = "smtp.mail.me.com";
                smtp.port = 587;
                mbsync = {
                  enable = true;
                  create = "both";
                  expunge = "both";
                  extraConfig = {
                    channel = {
                      Sync = "All";
                    };
                    account = {
                      Timeout = 120;
                      PipelineDepth = 1;
                    };
                  };
                };
                mu.enable = true;
                msmtp = {
                  enable = true;
                  extraConfig = {
                    tls_starttls = "on";
                  };
                };
              };
              # We will forward those to a "central" mail account.
              "gmail" = {
                address = "vinc.demeester@gmail.com";
                userName = "vinc.demeester@gmail.com";
                realName = "Vincent Demeester";
                passwordCommand = "${pkgs.passage}/bin/passage show mails/gmail/vinc.demeester";
                imap.host = "imap.gmail.com";
                smtp.host = "smtp.gmail.com";
                flavor = "gmail.com";
                # aerc.enable = true;
                msmtp = {
                  enable = true;
                  extraConfig = {
                    tls_starttls = "on";
                  };
                };
                # This is here for doing backup
                mbsync = {
                  enable = true;
                  create = "both";
                  expunge = "both";
                  # Sync everything *but* "[Gmail] All Mail" to get the "organized" view.
                  patterns = [ "*" "![Gmail]*" "[Gmail]/Sent Mail" "[Gmail]/Starred" "[Gmail]/Trash" "[Gmail]/Drafts" ];
                  extraConfig = {
                    channel = {
                      Sync = "All";
                    };
                    account = {
                      Timeout = 120;
                      PipelineDepth = 1;
                    };
                  };
                };
              };
              "redhat" = {
                # primary = true; # because it's work, but it's really just for notmuch
                address = "vdemeest@redhat.com";
                userName = "vdemeest@redhat.com";
                realName = "Vincent Demeester";
                passwordCommand = "${pkgs.passage}/bin/passage show mails/gmail/redhat";
                imap.host = "imap.gmail.com";
                smtp.host = "smtp.gmail.com";
                flavor = "gmail.com";
                mbsync = {
                  enable = true;
                  create = "both";
                  expunge = "both";
                  # Sync everything *but* "[Gmail] All Mail" to get the "organized" view.
                  patterns = [ "*" "!area/github" "!memo-list" "![Gmail]*" "[Gmail]/Sent Mail" "[Gmail]/Starred" "[Gmail]/Trash" "[Gmail]/Drafts" ];
                  extraConfig = {
                    channel = {
                      Sync = "All";
                    };
                    account = {
                      Timeout = 120;
                      PipelineDepth = 1;
                    };
                  };
                };
                mu.enable = true;
                # aerc.enable = true;
                msmtp = {
                  enable = true;
                  extraConfig = {
                    tls_starttls = "on";
                  };
                };
              };
            };
          };
        }
      ]
      # ++ optionals config.virtualisation.docker.enable [
      #   {
      #     home.packages = with pkgs; [ docker docker-compose dive ];
      #   }
      # ]
      #++ optionals config.profiles.redhat.enable [{
      #  home.file.".local/share/applications/redhat-vpn.desktop".source = ./redhat/redhat-vpn.desktop;
      #  home.packages = with pkgs; [ gnome3.zenity oathToolkit ];
      #}]
    );
}
