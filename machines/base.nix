{ pkgs, ...}:

{
  imports = [ ../modules/module-list.nix ];
  programs = {
    home-manager = {
      enable = true;
    };
  };
  home.file.".nix-channels".source = ../assets/nix-channels;
  home.packages = with pkgs; [
    direnv
    enchive
    entr
    exa
    fd
    htop
    scripts
    tree
  ];
  xdg.configFile."nr/default" = {
    text = builtins.toJSON [
      {cmd = "ncdu";} {cmd = "sshfs";} {cmd = "gotop";} {cmd = "pandoc";} { cmd = "nix-review"; }
      {cmd = "lspci"; pkg = "pciutils";}
      {cmd = "lsusb"; pkg = "usbutils";}
      {cmd = "9"; pkg = "plan9port"; }
      {cmd = "wakeonlan"; pkg = "python36Packages.wakeonlan";}
      {cmd = "beet"; pkg = "beets";}
      {cmd = "http"; pkg = "httpie"; }
      {cmd = "nix-prefetch-git"; pkg = "nix-prefetch-scripts";}
      {cmd = "nix-prefetch-hg"; pkg = "nix-prefetch-scripts";}
      {cmd = "op"; pkg = "_1password"; chan = "unstable";}
    ];
    onChange = "${pkgs.nur.repos.vdemeester.nr}/bin/nr default";
  };
}
