self: pkgs: rec {

  myEmacs = customEmacsPackages.emacsWithPackages pkgs.emacs;
  emacsPackagesNg = pkgs.emacsPackagesNgGen pkgs.emacs;
  customEmacsPackages =
    emacsPackagesNg.overrideScope (myEmacsPackageOverrides pkgs.emacs);

  compileEmacsFiles  = pkgs.callPackage ./emacs/builder.nix;

  fetchFromEmacsWiki = pkgs.callPackage ({ fetchurl, name, sha256 }:
    fetchurl {
      inherit sha256;
      url = "https://www.emacswiki.org/emacs/download/" + name;
    });

  compileEmacsWikiFile = { name, sha256, buildInputs ? [], patches ? [] }:
  compileEmacsFiles {
    inherit name buildInputs patches;
    src = fetchFromEmacsWiki { name = name; sha256 = sha256; };
  };

  compileLocalFile = name: compileEmacsFiles {
    inherit name; src = ./emacs + ("/" + name);
  };
  
  myEmacsPackageOverrides = emacs: super: self: with self;
    let withPatches = pkg: patches:
    overrideDerivation pkg (attrs: { inherit patches; }); in

    super.melpaPackages // {

    inherit (pkgs) fetchurl fetchgit fetchFromGitHub;
    inherit (pkgs) stdenv;
    inherit (stdenv) mkDerivation lib;
    inherit (lib) overrideDerivation;

    /*
    edit-env        = compileLocalFile "edit-env.el";
    edit-var        = compileLocalFile "edit-var.el";
    ox-extra        = compileLocalFile "ox-extra.el";
    rs-gnus-summary = compileLocalFile "rs-gnus-summary.el";
    supercite       = compileLocalFile "supercite.el";
    */

    pass = withPatches melpaPackages.pass
      [ ./emacs/patches/pass.patch ];

    password-store = withPatches melpaPackages.password-store
      [ ./emacs/patches/password-store.patch ];

    minions = melpaBuild {
      pname = "minions";
      version = "20180321.749";
      src = fetchFromGitHub {
        owner = "tarsius";
        repo = "minions";
        rev = "acac7fb0b04ffdf243775231690956d163474177";
        sha256 = "1065asbg1h2chd34cbja2pir93r5y8gp3wv3jv6sf5csa6yqk6c7";
      };
      recipeFile = fetchurl {
        url = "https://raw.githubusercontent.com/melpa/melpa/e5cfaa4b5fda97054d45691fad9d79b559f2df14/recipes/minions";
        sha256 = "1065asbg1h2chd34cbja2pir93r5y8gp3wv3jv6sf5csa6yqk6ch";
        name = "minions";
      };
      packageRequires = [ emacs ];
      meta = {
        homepage = "https://melpa.org/#/minions";
        license = lib.licenses.free;
      };
    };
    
    counsel-org-clock = melpaBuild {
      pname = "counsel-org-clock";
      version = "20180501.1021";
      src = fetchFromGitHub {
        owner = "akirak";
        repo = "counsel-org-clock";
        rev = "1b57912f117c8689224faa40842739eb6624d935";
        sha256 = "1065asbg1h2chd34cbja2pir93r5y8gp3wv3jv6sf5csa6yqk6h7";
      };
      recipeFile = fetchurl {
        url = "https://raw.githubusercontent.com/melpa/melpa/d21e10ba82b4ae0f8101031be16bc5f7e80ba5d5/recipes/counsel-org-clock";
        sha256 = "1065asbg1h2chd34cbja2pir93r5y8gp3wv3jv6sf5csa6yqk6cf";
        name = "counsel-org-clock";
      };
      packageRequires = [ emacs ];
      meta = {
        homepage = "https://melpa.org/#/counsel-org-clock";
        license = lib.licenses.free;
      };
    };

    ascii = compileEmacsWikiFile {
      name = "ascii.el";
      sha256 = "05fjsj5nmc05cmsi0qj914dqdwk8rll1d4dwhn0crw36p2ivql75";
      # date = 2018-02-21T17:21:27-0800;
    };

    emacs-load-time = compileEmacsFiles {
      name = "emacs-load-time";
      src = fetchFromGitHub {
        owner = "fniessen";
        repo = "emacs-load-time";
        rev = "9d31686a76e9792bd06e49ff77c662065ded015c";
        sha256 = "0zhrfidcxqfld7y67pysdlcvrprrka9sq8065ygqx5yxjb7mxs32";
        # date = 2014-10-10T16:52:58+02:00;
      };
    };
  };
}
