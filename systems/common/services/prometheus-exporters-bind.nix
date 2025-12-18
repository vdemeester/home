_: {
  services.prometheus.exporters.bind = {
    enable = true;
    port = 9009;
    bindURI = "http://localhost:8053";
  };
}
