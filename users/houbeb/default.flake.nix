{ inputs, ... }: {
  users.users.houbeb = {
    createHome = true;
    description = "Houbeb Ben Othmene";
    extraGroups = [ "wheel" ];
    isNormalUser = true;
    openssh.authorizedKeys.keys = [
      "…"
    ];
  };
  # Home-manager "magic"
  home-manager.users.houbeb = inputs.self.internal.homeManagerConfigurations."houbeb";
}
