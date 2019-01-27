{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.git;
  ca-bundle_crt = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
in
{
  options = {
    profiles.git = {
      enable = mkOption {
        default = false;
        description = "Enable git profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        gist
        git-lfs
        gitAndTools.gitflow
        gitAndTools.hub
        gitAndTools.tig
        nur.repos.vdemeester.prm
        nur.repos.vdemeester.ape
      ];
      programs.git = {
        enable = true;
    
        userName = "Vincent Demeester";
        userEmail = "vincent@sbr.pm";
    
        signing = {
          key = "6EB699A3";
          signByDefault = false;
        };
    
        aliases = {
          b = "branch --color -v";
          br = "branch";
          ci = "commit --signoff";
          co = "checkout";
          conflicts = "!git ls-files --unmerged | cut -c51- | sort -u | xargs $EDITOR";
          ca = "commit --amend";
          wdiff = "diff --color-words";
          unstage = "reset HEAD";
          lg = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative";
          lga = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative --branches --remotes";
          lol = "log --pretty=oneline --abbrev-commit --graph --decorate";
          ls-ignored = "ls-files --exclude-standard --ignored --others";
          resolve = "!git ls-files --unmerged | cut -c51- | sort -u | xargs git add";
          su = "submodule update --init --recursive";
          st = "status";
          w = "status -sb";
        };
        extraConfig = {
          core = {
            editor = "${pkgs.emacs}/bin/emacsclient -t";
          };
          forge = {
            remote = "upstream";
          };
          color = {
            status      = "auto";
            diff        = "auto";
            branch      = "auto";
            interactive = "auto";
            ui          = "auto";
            sh          = "auto";
          };
          "color \"branch\"" = {
            current = "cyan reverse";
            local = "cyan";
            remote = "green";
          };
          "color \"diff\"" = {
            current = "white reverse";
            frag = "magenta reverse";
            old = "red";
            new = "green";
          };
          "color \"status\"" = {
            added = "green";
            changed = "yellow";
            untracked = "red";
          };
          hub = {
            protocol = true;
          };
          pull = {
            rebase = true;
          };
          push = {
            default = "upstream";
            recurseSubmodules = "check";
          };
          rebase = {
            autosquash = true;
          };
          advice = {
            statusHints = false;
            pushNonFastForward = false;
          };
          http = {
            sslCAinfo = "${ca-bundle_crt}";
            sslverify = true;
          };
          github.user = "vdemeester";
          "filter \"lfs\"" = {
            clean = "${pkgs.git-lfs}/bin/git-lfs clean -- %f";
            smudge = "${pkgs.git-lfs}/bin/git-lfs smudge --skip -- %f";
            required = true;
          };
          "url \"git@github.com:\"".insteadOf = "git://github.com/";
        };
        
        includes = [
          {
            path = "~/.config/git/config.d/redhat.gitconfig";
            condition = "gitdir:~/src/github.com/knative/";
          }
          {
            path = "~/.config/git/config.d/redhat.gitconfig";
            condition = "gitdir:~/src/k8s.io/";
          }
          {
            path = "~/.config/git/config.d/redhat.gitconfig";
            condition = "gitdir:~/src/github.com/minishift/";
          }
          {
            path = "~/.config/git/config.d/redhat.gitconfig";
            condition = "gitdir:~/src/github.com/operator-framework/";
          }
          {
            path = "~/.config/git/config.d/redhat.gitconfig";
            condition = "gitdir:~/src/github.com/openshift/";
          }
          {
            path = "~/.config/git/config.d/redhat.gitconfig";
            condition = "gitdir:~/src/github.com/redhat-developer/";
          }
          {
            path = "~/.config/git/config.d/redhat.gitconfig";
            condition = "gitdir:~/src/github.com/containers/";
          }
        ];
        ignores = [
          "*.elc" "*.vo" "*.aux" "*.v.d" "*.o" "*.a" "*.la" "*.so" "*.dylib"
          "*.pyc" "*.pyo" ".idea" "*.iml"
          "*~" "#*#" ".makefile" ".clean"
        ];
      };
      xdg.configFile."git/config.d/redhat.gitconfig".source = ./assets/git/redhat.gitconfig;
      xdg.configFile."nr/git" = {
        text = builtins.toJSON [
          {cmd = "mr";}
          {cmd = "grv"; pkg = "gitAndTools.grv";}
          {cmd = "git-annex"; pkg = "gitAndTools.git-annex";}
          {cmd = "git-appraise"; pkg = "gitAndTools.git-appraise"; chan = "unstable";}
        ];
        onChange = "${pkgs.nur.repos.vdemeester.nr}/bin/nr git";
      };
    }
    (mkIf config.profiles.fish.enable{
      programs.fish.shellAbbrs = {
        gs = "git status";
        gb = "git b";
        gfr = "git fetch -p --all;and git rebase -S --autostash";
      };
    })
  ]);
}
