{
  buildGoModule,
  lib,
}:

buildGoModule {
  pname = "battery-monitor";
  version = "0.1.0";

  src = ./.;

  vendorHash = "sha256-K9lqUdIR2EpLEQMTrlv6bGj3IpwK/yaFCqY7S7nuXg4=";

  meta = {
    description = "Battery monitoring tool for laptops";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
    mainProgram = "battery-monitor";
  };
}
