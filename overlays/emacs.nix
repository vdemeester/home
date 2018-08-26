self: super:

let
  inherit (super) fetchurl fetchgit fetchFromGitHub stdenv melpaBuild;
  inherit (stdenv) lib mkDerivation;

  overrides = {
    minions = melpaBuild {
      pname = "minions";
      ename = "minions";
      version = "20180709.1012";
      src = fetchFromGitHub {
        owner = "tarsius";
        repo = "minions";
        rev = "2f5e73e15d0021e7ba26cf09f1cd2734b018fb69";
        sha256 = "12acfjmk6n40k5mb2hy1izbk483y83bc3d54r76l750sbm3kpdar";
      };
      recipe = fetchurl {
        url = "https://raw.githubusercontent.com/milkypostman/melpa/769a2167d7f6dfdbbfda058ddea036f80b97d230/recipes/minions";
        sha256 = "0ximlj93yp6646bh99r2vnayk15ky26sibrmrqqysfw1pzs4a940";
        name = "recipe";
      };
      packageRequires = [ super.dash super.emacs ];
      meta = {
        homepage = "https://melpa.org/#/minions";
        license = lib.licenses.free;
      };
    };
  };

  melpaPackages = super // overrides;

in 
  melpaPackages // { inherit melpaPackages; }
