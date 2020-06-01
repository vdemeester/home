{ pkgs, ... }:

{
  xdg.configFile."nr/gcloud" = {
    text = builtins.toJSON [
      { cmd = "gcloud"; pkg = "google-cloud-sdk"; }
      { cmd = "gcsfuse"; }
    ];
    onChange = "${pkgs.my.nr}/bin/nr gcloud";
  };
}
