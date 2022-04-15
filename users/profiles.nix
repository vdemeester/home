inputs:

let
  inherit (inputs) self;
  inherit (self.lib) mkHome extraSpecialArgs;

  sharedModules = [
    # ../.
    # ../files
    # ../shell
    # ../games.nix
    # ../media.nix
  ];

  homeImports = {
    "vincent@naruhodo" = sharedModules ++ [ ../wayland ./mihai-io ../editors/emacs ];
    # "vincent@aomi" = sharedModules ++ [ ../wayland ./mihai-tosh ];
    "vincent@okinawa" = sharedModules ++ [ ../wayland ./mihai-tosh ];
    server = [ ../cli.nix ];
  };
in
{
  inherit homeImports extraSpecialArgs;

  homeConfigurations = {
    "vincent@naruhodo" = {
      username = "vincent";
      extraModules = homeImports."vincent@naruhodo";
    };
    "vincent@okinawa" = {
      username = "vincent";
      extraModules = homeImports."vincent@naruhodo";
    };
    "server" = {
      username = "vincent";
      extraModules = homeImports.server;
    };
  };
}
