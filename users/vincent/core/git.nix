{ config, lib, pkgs, ... }:

with lib;
let
  ca-bundle_crt = "/etc/ssl/certs/ca-bundle.crt";
  redhat_folders = [
    "src/github.com/containers"
    "src/github.com/google"
    "src/github.com/knative"
    "src/github.com/kubernetes"
    "src/github.com/openshift"
    "src/github.com/openshift-knative"
    "src/github.com/openshift-pipelines"
    "src/github.com/operator-framework"
    "src/github.com/redhat-developer"
    "src/github.com/tektoncd"
    "src/gitlab.cee.redhat.com"
    "src/gitlab.corp.redhat.com"
    "src/k8s.io"
    "src/osp"
    "src/pkg.devel.redhat.com"
    "src/tektoncd"
  ];
in
{
  home.packages = with pkgs; [
    gist
    git-lfs
    git-review
    gitAndTools.git-annex
    gitAndTools.hub
    gitAndTools.gh
    gitAndTools.git-appraise
    mr
    #my.prm
    #my.ape
    difftastic
  ];
  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;

    userName = "Vincent Demeester";
    userEmail = "vincent@sbr.pm";

    signing = {
      key = "6EB699A3";
      signByDefault = false;
    };

    aliases = {
      b = "branch --color -v";
      br = "branch";
      ca = "commit --amend";
      ci = "commit --signoff";
      co = "checkout";
      conflicts = "!git ls-files --unmerged | cut -c51- | sort -u | xargs $EDITOR";
      dft = "difftool";
      lg = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative";
      lga = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative --branches --remotes";
      lol = "log --pretty=oneline --abbrev-commit --graph --decorate";
      ls-ignored = "ls-files --exclude-standard --ignored --others";
      resolve = "!git ls-files --unmerged | cut -c51- | sort -u | xargs git add";
      st = "status";
      su = "submodule update --init --recursive";
      unstage = "reset HEAD";
      w = "status -sb";
      wdiff = "diff --color-words";
    };
    attributes = [
      "*.org   diff=org"
    ];
    extraConfig = {
      core = {
        #editor = "${pkgs.emacs}/bin/emacsclient -t";
      };
      color = {
        status = "auto";
        diff = "auto";
        branch = "auto";
        interactive = "auto";
        ui = "auto";
        sh = "auto";
      };
      "color.branch" = {
        current = "cyan reverse";
        local = "cyan";
        remote = "green";
      };
      "color.diff" = {
        current = "white reverse";
        frag = "magenta reverse";
        old = "red";
        new = "green";
      };
      "color.status" = {
        added = "green";
        changed = "yellow";
        untracked = "red";
      };
      diff = {
        external = "difft";
        tool = "difftastic";
      };
      "diff.org" = {
        xfuncname = "\"^\\\\*+.*\"";
      };
      difftool = {
        prompt = false;
      };
      "difftool.difftastic" = {
        cmd = "difft \"$LOCAL\" \"$REMOTE\"";
      };
      pager = {
        difftool = true;
      };
      forge = {
        remote = "upstream";
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
      credential = {
        "https://github.com" = {
          helper = "!${pkgs.gh}/bin/gh auth git-credential";
        };
        "https://gist.github.com" = {
          helper = "!${pkgs.gh}/bin/gh auth git-credential";
        };
      };
      github.user = "vdemeester";
      "filter \"lfs\"" = {
        clean = "${pkgs.git-lfs}/bin/git-lfs clean -- %f";
        smudge = "${pkgs.git-lfs}/bin/git-lfs smudge --skip -- %f";
        required = true;
      };
      "url \"git@github.com:\"".insteadOf = "git://github.com/";
    };

    includes = [ ] ++ lists.forEach redhat_folders (x: {
      path = "${config.xdg.configHome}/git/config.d/redhat.gitconfig";
      condition = "gitdir:${config.home.homeDirectory}/${x}/**";
    });
    ignores = [
      "*.elc"
      "*.vo"
      "*.aux"
      "*.v.d"
      "*.o"
      "*.a"
      "*.la"
      "*.so"
      "*.dylib"
      "*.pyc"
      "*.pyo"
      ".idea"
      "*.iml"
      "*~"
      "#*#"
      ".makefile"
      ".clean"
    ];
  };
  xdg.configFile."git/config.d/redhat.gitconfig".source = ./git/redhat.gitconfig;
}
