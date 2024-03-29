{ config, lib, pkgs, ... }:
let
  # To get fingerprint : autorandr --fingerprint
  lg_ultrawide_curved = "00ffffffffffff001e6df6760cd105000a1b010380502278eaca95a6554ea1260f50542108007140818081c0a9c0b300d1c081000101e77c70a0d0a0295030203a00204f3100001a9d6770a0d0a0225030203a00204f3100001a000000fd00383d1e5a20000a202020202020000000fc004c4720554c545241574944450a012902031ef12309070749100403011f13595a128301000067030c00200038409f3d70a0d0a0155030203a00204f3100001a7e4800e0a0381f4040403a00204f31000018011d007251d01e206e285500204f3100001e8c0ad08a20e02d10103e9600204f31000018000000ff003731304e544a4a42373139360a0000000000000023";
  thinkpadt480s = "00ffffffffffff000daec91400000000081a0104951f11780228659759548e271e505400000001010101010101010101010101010101b43b804a71383440503c680035ad10000018000000fe004e3134304843412d4541420a20000000fe00434d4e0a202020202020202020000000fe004e3134304843412d4541420a20003e";
  thinkpadp1 = "00ffffffffffff000dae0c15000000002a1c0104b522137802ee95a3544c99260f505400000001010101010101010101010101010101363680a0703820405036680058c110000018363680a0703820405036680058c110000018000000fe00434d4e0a202020202020202020000000fe004e3135364843452d474e310a2001d102030f00e3058000e60605016a6a2400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005435313343363031415230320000000000000000000000000000000000de";
in
{
  programs.autorandr = {
    enable = true;
    hooks.postswitch."notify-i3" = "${config.xsession.windowManager.i3.package}/bin/i3-msg restart";
    hooks.postswitch."reset-background" = "systemctl --user start random-background.service";
    profiles = {
      p1-on-the-move = {
        fingerprint = {
          eDP-1 = thinkpadp1;
        };
        config = {
          eDP-1 = {
            enable = true;
            primary = true;
            position = "0x0";
            mode = "1920x1080";
          };
        };
      };
      p1-home = {
        fingerprint = {
          eDP-1 = thinkpadp1;
          DP-3-1 = lg_ultrawide_curved;
        };
        config = {
          eDP-1 = {
            enable = false;
          };
          DP-3-1 = {
            enable = true;
            primary = true;
            mode = "3440x1440";
            position = "0x0";
          };
        };
      };
      t480-on-the-move = {
        fingerprint = {
          eDP-1 = thinkpadt480s;
        };
        config = {
          eDP-1 = {
            enable = true;
            primary = true;
            position = "0x0";
            mode = "1920x1080";
          };
        };
      };
      t480-home = {
        fingerprint = {
          eDP-1 = thinkpadt480s;
          DP-1-1 = lg_ultrawide_curved;
        };
        config = {
          eDP-1 = {
            enable = false;
          };
          DP-1-1 = {
            enable = true;
            primary = true;
            mode = "3440x1440";
            position = "0x0";
          };
        };
      };
    };
  };
}
