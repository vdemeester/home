{ pkgs
, ...
}:
{
  environment.systemPackages = with pkgs; [
    age
    age-plugin-tpm
    agenix
    passage
  ];
}
