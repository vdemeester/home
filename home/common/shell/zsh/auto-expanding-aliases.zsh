# Definition of abbrev-alias for auto-expanding aliases
# Autoexpand some aliases
typeset -ga _vbe_abbrevations
abbrev-alias() {
    alias $1
    _vbe_abbrevations+=(${1%%\=*})
}
_vbe_zle-autoexpand() {
    local -a words; words=(${(z)LBUFFER})
    if (( ${#_vbe_abbrevations[(r)${words[-1]}]} )); then
        zle _expand_alias
    fi
    zle magic-space
}
zle -N _vbe_zle-autoexpand
bindkey -M emacs " " _vbe_zle-autoexpand
bindkey -M emacs "^ " magic-space
bindkey -M isearch " " magic-space

# Correct common typos
(( $+commands[git] )) && abbrev-alias gti=git
(( $+commands[grep] )) && abbrev-alias grpe=grep
(( $+commands[sudo] )) && abbrev-alias suod=sudo
(( $+commands[ssh] )) && abbrev-alias shs=ssh

# Save a few keystrokes
(( $+commands[git] )) && abbrev-alias gls="git ls-files"
(( $+commands[ip] )) && {
  abbrev-alias ip6='ip -6'
  abbrev-alias ipb='ip -brief'
}

abbrev-alias tailf="tail -F"

(( $+commands[mpv] )) && abbrev-alias mpva="mpv --no-video"

# System init-related aliases
# shellcheck disable=SC1072,SC1073
() {
    local cmd
    local -a cmds
    cmds=(start stop reload restart status)

    if [[ -d /run/systemd/system ]]; then
        # systemd
        for cmd ($cmds) {
            abbrev-alias $cmd="${(%):-%(#..sudo )}systemctl $cmd"
            abbrev-alias u$cmd="systemctl --user $cmd"
        }
            else
                # generic service
                for cmd ($cmds) {
                    function $cmd() {
                        name=$1 ; shift
                        ${(%):-%(#..sudo)} service $name $0 "$@"
                    }
                    (( $+functions[compdef] )) && compdef _services $cmd
                }
                fi
}

# grep aliases
() {
    # If GNU grep is available, use it
    local grep=grep
    (( $+commands[ggrep] )) && grep=ggrep # GNU grep

    # Check if grep supports colors
    local colors="--color=auto"
    $grep -q $colors . <<< yes 2> /dev/null || colors=""

    # Declare aliases
    alias grep="command ${grep} ${colors}"
    abbrev-alias rgrep="grep -r"
    abbrev-alias egrep="grep -E"
    abbrev-alias fgrep="grep -F"
    # --color=auto doesn't work. See https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=792135
    (( $+commands[zgrep] )) && alias zgrep="GREP=${grep} command zgrep ${colors}"
}

# (( $+commands[emacsclient] * $+commands[git] )) && magit() {
#         local root=$(git rev-parse --show-toplevel)
#         [[ -n $root ]] || return
#         emacsclient -e "(progn
#                     (select-frame-set-input-focus
#                       (window-frame
#                         (get-buffer-window
#                            (magit-status \"${root}\"))))
#                     (delete-other-windows))"
# }

(( $+commands[nix] )) && nixpkgs() {
    cmd=$1
    shift
    nix run nixpkgs\#${cmd} -- "$@"
}

v() {
    case $(file --brief --mime-type $1 2> /dev/null) in
        image/svg+xml) ;;
        image/*)
            (( $+commands[nsxiv] )) && ${I3SOCK+i3-tabbed} nsxiv $1
            return
            ;;
        video/*)
            (( $+commands[mpv] )) && ${I3SOCK+i3-tabbed} mpv --no-fs $1
            return
    esac
    if (( $+commands[bat] )); then
        if (( ! $# )); then
            gzip -cdfq | bat
        else
            for f in "$@"; do
                gzip -cdfq -- $f | bat --file-name ${f%.gz}
            done
        fi
    elif (( $+commands[less] )); then
        gzip -cdfq -- "$@" | less -FX
    elif (( $+commands[zmore] )); then
        zmore "$@"
    elif (( $+commands[more] )); then
        gzip -cdfq -- "$@" | more
    else
        gzip -cdfq -- "$@"
    fi
}

function clean() {}

if [[ -d ${HOME}/src/github.com/chmouel/jayrah ]]; then
    alias jayrah="uv --directory=${HOME}/src/github.com/chmouel/jayrah run jayrah"
fi
