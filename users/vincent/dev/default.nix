{ pkgs, ... }:

{
  imports = [
    # ./emacs.nix
    ./go.nix
    ./js.nix
    ./lisp.nix
    ./mr.nix
    ./nix.nix
    ./python.nix
  ];

  home.extraOutputsToInstall = [ "doc" "info" "devdoc" ];

  home.packages = with pkgs; [
    binutils
    cmake
    devenv
    difftastic
    fswatch
    gnumake
    gron
    jq
    moreutils
    shfmt
    vale
    yq-go
    fossil
    pre-commit
    # temporary
    vscode
  ];

  home.file.".ignore".text = ''
    *.swp
    *~
    **/VENDOR-LICENSE
  '';

  home.file.gdbinit = {
    target = ".gdbinit";
    text = ''
      set auto-load safe-path /
    '';
  };

  # FIXME move this to a "work.redhat" configuration.
  # Upstream Tekton
  # home.file."src/tektoncd/.envrc".text = ''
  #   eval "$(lorri direnv)"
  # '';
  # home.file."src/tektoncd/shell.nix".source = ../../../shells/tekton.nix;
  # Downstream OSP
  # home.file."src/osp/.envrc".text = ''
  #   eval "$(lorri direnv)"
  # '';
  # home.file."src/osp/shell.nix".source = ../../../shells/osp.nix;
}
