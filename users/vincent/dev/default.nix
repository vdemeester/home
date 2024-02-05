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
    yamllint
    markdownlint
    vale
    codespell
    tmate
    # temporary
    # (vscode-with-extensions.override
    #   {
    #     vscodeExtensions = with vscode-extensions; pkgs.vscode-utils.extensionsFromVscodeMarketplace [
    #       # {
    #       #   name = "tooltitude";
    #       #   publisher = "tooltitudeteam";
    #       #   version = "0.43.2";
    #       #   sha256 = "sha256-d4h+kEgvSjtLva6c4UWRLR2jP+ydieDzAHkimbptV48=";
    #       # }
    #     ] ++ [
    #       ms-vsliveshare.vsliveshare
    #       golang.go
    #       github.codespaces
    #       editorconfig.editorconfig
    #       eamodio.gitlens
    #       github.copilot
    #       ms-vscode-remote.remote-ssh
    #       ms-vscode-remote.remote-containers
    #       redhat.vscode-yaml
    #     ];
    #   })
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
