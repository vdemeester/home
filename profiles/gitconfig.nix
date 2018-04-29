{ configs, pkgs, ...}:

{
  environment.systemPackages = with pkgs; [
    git
  ];
	environment.etc."gitconfig" = rec { text = ''
[alias]
    co = checkout
    st = status
    ci = commit --signoff
    cia = commit --amend
    ciad = commit --amend --date=\"$(date -R)\"
    ciads = commit --amend --date=\"$(date -R)\" -S
    civ = commit -v
    cis = commit --signoff -S
    cids = commit --signoff --date=\"$(date -R)\"
    wdiff = diff --color-words
    br = branch
    unstage = reset HEAD
    lg = log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative
    lga = log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative --branches --remotes
    lol = log --pretty=oneline --abbrev-commit --graph --decorate
    conflicts = !git ls-files --unmerged | cut -c51- | sort -u | xargs $EDITOR
    resolve = !git ls-files --unmerged | cut -c51- | sort -u | xargs git add
[color]
	branch = auto
	diff = auto
	status = auto
[color "branch"]
	current = cyan reverse
	local = cyan
	remote = green
[color "diff"]
	meta = white reverse
	frag = magenta reverse
	old = red 
	new = green 
[color "status"]
	added = green
	changed = yellow
	untracked = red
[core]
	excludesfile = ~/.gitignore.global
[push]
	default = matching
[merge]
    tool = vimdiff

[user]
    name = Vincent Demeester
    email = vincent@sbr.pm

[http]
        cookiefile = /home/vincent/.gitcookies
	
[url "git@github.com:"]
    pushInsteadOf = git://github.com/
	''; };
}
