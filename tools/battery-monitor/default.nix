{
  buildGoModule,
  lib,
}:

buildGoModule rec {
  pname = "battery-monitor";
  version = "0.1.0";

  src = ./.;

  vendorHash = "sha256-K9lqUdIR2EpLEQMTrlv6bGj3IpwK/yaFCqY7S7nuXg4=";

  meta = with lib; {
    description = "Battery monitoring tool for laptops";
    license = licenses.mit;
    platforms = platforms.linux;
    mainProgram = "battery-monitor";
  };
}
