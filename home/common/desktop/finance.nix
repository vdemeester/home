{ pkgs, ... }:
let
  financeAliases = {
    fin = "hledger";
    finui = "hledger-ui";
    finweb = "hledger-web --serve";
    finimport = "~/desktop/finance/scripts/import-csv.sh";
    finrec = "~/desktop/finance/scripts/reconcile.sh";
  };
in
{
  home.packages = with pkgs; [
    hledger
    hledger-ui
    hledger-web
  ];

  home.file.".hledgerrc".text = ''
    # Default journal file
    --file ~/desktop/finance/main.journal
  '';

  programs.bash.shellAliases = financeAliases;
  programs.zsh.shellAliases = financeAliases;
}
