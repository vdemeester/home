{ config, ... }:
{
  aliases = {
    mkdir = ''mkdir --parents --verbose'';
    rm = ''rm --interactive'';
    cp = ''cp --interactive'';
    mv = ''mv --interactive'';
    gcd = ''cd (git root)'';
    # ls = ''exa'';
    ll = ''ls -l'';
    la = ''ls -a'';
    l = ''ls -lah'';
    # t = ''exa --tree --level=2'';
    map = ''xargs -n1'';
    k = ''kubectl'';
    wget = ''wget -c --hsts-file=${config.xdg.dataHome}/wget-hsts'';
  };

  env = ''
    export PATH=$HOME/bin:$PATH
    export LESSHISTFILE="${config.xdg.dataHome}/less_history"
    export WEBKIT_DISABLE_COMPOSITING_MODE=1;
    export PATH=$HOME/bin:$PATH
    if [ -d $HOME/.krew/bin ]; then
      export PATH=$HOME/.krew/bin:$PATH
    fi
    # TODO Move somewhere else
    export TLDR_CACHE_DIR="$XDG_CACHE_HOME"/tldr 
  '';

  historySize = 10000;
}
