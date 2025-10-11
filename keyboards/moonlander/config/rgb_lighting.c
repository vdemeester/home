/* Copyright 2020 ZSA Technology Labs, Inc <@zsa>
 * Copyright 2020 Jack Humbert <jack.humb@gmail.com>
 * Copyright 2020 Christopher Courtney, aka Drashna Jael're  (@drashna) <drashna@live.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include QMK_KEYBOARD_H
#include "layers.h"
#include "rgb_lighting.h"

extern rgb_config_t rgb_matrix_config;

// Line 2 home-row (7 12 17 22 [27] and [63] 58 53 48 43)
void my_set_rgb_matrix_color_homerow(uint8_t red, uint8_t green, uint8_t blue) {
  rgb_matrix_set_color(7, red, green, blue);
  rgb_matrix_set_color(12, red, green, blue);
  rgb_matrix_set_color(17, red, green, blue);
  rgb_matrix_set_color(22, red, green, blue);
    
  rgb_matrix_set_color(58, red, green, blue);
  rgb_matrix_set_color(53, red, green, blue);
  rgb_matrix_set_color(48, red, green, blue);
  rgb_matrix_set_color(43, red, green, blue);
}

void my_set_rgb_matrix_color_line(int line,  uint8_t red, uint8_t green, uint8_t blue) {
  if (line == 0 || line == 1 || line == 2) {
    // Left
    rgb_matrix_set_color(line, red, green, blue);
    rgb_matrix_set_color(line+5, red, green, blue);
    rgb_matrix_set_color(line+10, red, green, blue);
    rgb_matrix_set_color(line+15, red, green, blue);
    rgb_matrix_set_color(line+20, red, green, blue);
    rgb_matrix_set_color(line+25, red, green, blue);
    /* rgb_matrix_set_color(line+29, red, green, blue); */
    // Right
    /* rgb_matrix_set_color(line+65, red, green, blue); */
    rgb_matrix_set_color(line+61, red, green, blue);
    rgb_matrix_set_color(line+56, red, green, blue);
    rgb_matrix_set_color(line+51, red, green, blue);
    rgb_matrix_set_color(line+46, red, green, blue);
    rgb_matrix_set_color(line+41, red, green, blue);
    rgb_matrix_set_color(line+36, red, green, blue);
  } else if (line == 3) {
    // Left
    rgb_matrix_set_color(3, red, green, blue);
    rgb_matrix_set_color(8, red, green, blue);
    rgb_matrix_set_color(13, red, green, blue);
    rgb_matrix_set_color(18, red, green, blue);
    rgb_matrix_set_color(23, red, green, blue);
    rgb_matrix_set_color(28, red, green, blue);
    // Right
    rgb_matrix_set_color(64, red, green, blue);
    rgb_matrix_set_color(59, red, green, blue);
    rgb_matrix_set_color(54, red, green, blue);
    rgb_matrix_set_color(49, red, green, blue);
    rgb_matrix_set_color(44, red, green, blue);
    rgb_matrix_set_color(39, red, green, blue);
  } else if (line == 4) {
    // Left
    /* rgb_matrix_set_color(4, red, green, blue); */
    /* rgb_matrix_set_color(9, red, green, blue); */
    /* rgb_matrix_set_color(14, red, green, blue); */
    /* rgb_matrix_set_color(19, red, green, blue); */
    rgb_matrix_set_color(24, red, green, blue);
    rgb_matrix_set_color(32, red, green, blue);
    rgb_matrix_set_color(33, red, green, blue);
    /* rgb_matrix_set_color(34, red, green, blue); */
    rgb_matrix_set_color(35, red, green, blue);
    // Right
    rgb_matrix_set_color(60, red, green, blue);
    /* rgb_matrix_set_color(55, red, green, blue); */
    /* rgb_matrix_set_color(50, red, green, blue); */
    /* rgb_matrix_set_color(45, red, green, blue); */
    /* rgb_matrix_set_color(40, red, green, blue); */
    rgb_matrix_set_color(68, red, green, blue);
    rgb_matrix_set_color(69, red, green, blue);
    /* rgb_matrix_set_color(70, red, green, blue); */
    rgb_matrix_set_color(71, red, green, blue);
  }
}

// matrix, to set key with. maybe there is a better way but used ascii generator so i can easily set those visually
//    ┌────┬────┬────┬────┬────┬────┬────┐             ┌────┬────┬────┬────┬────┬────┬────┐
//    │ 0  │ 5  │ 10 │ 15 │ 20 │ 25 │ 29 │             │ 65 │ 61 │ 56 │ 51 │ 46 │ 41 │ 36 │
//    ├────┼────┼────┼────┼────┼────┼────┤             ├────┼────┼────┼────┼────┼────┼────┤
//    │ 1  │ 6  │ 11 │ 16 │ 21 │ 26 │ 30 │             │ 66 │ 62 │ 57 │ 52 │ 47 │ 42 │ 37 │
//    ├────┼────┼────┼────┼────┼────┼────┤             ├────┼────┼────┼────┼────┼────┼────┤
//    │ 2  │ 7  │ 12 │ 17 │[22]│ 27 │ 31 │             │ 67 │ 63 │[58]│ 53 │ 48 │ 43 │ 38 │
//    ├────┼────┼────┼────┼────┼────┼────┘             └────┼────┼────┼────┼────┼────┼────┤
//    │ 3  │ 8  │ 13 │ 18 │ 23 │ 28 │                       │ 64 │ 59 │ 54 │ 49 │ 44 │ 39 │
//    ├────┼────┼────┼────┼────┼────┼─────────┐   ┌─────────┼────┼────┼────┼────┼────┼────┤
//    │ 4  │ 9  │ 14 │ 19 │ 24 │    │   35    │   │   71    │    │ 60 │ 55 │ 50 │ 45 │ 40 │
//    └────┴────┴────┴────┴────┼────┼────┬────┤   ├────┬────┼────┼────┴────┴────┴────┴────┘
//                             │ 32 │ 33 │ 34 │   │ 70 │ 69 │ 68 │
//                             └────┴────┴────┘   └────┴────┴────┘
// Runs constantly in the background, in a loop.
bool rgb_matrix_indicators_user(void) {
  if (rgb_matrix_get_flags() & (LED_FLAG_KEYLIGHT | LED_FLAG_MODIFIER)) {
    switch (get_highest_layer(layer_state)) {
    case BEPO:
      rgb_matrix_set_color_all(RGB_OFF);
      // Line 1
      my_set_rgb_matrix_color_line(1,213,144,0);
      // Line 2
      my_set_rgb_matrix_color_line(2,27,213,0);
      my_set_rgb_matrix_color_homerow(206,255,206);
      // Line 3
      my_set_rgb_matrix_color_line(3,0,255,167);
      // Line 4
      my_set_rgb_matrix_color_line(4,0,113,255);
      break;
    case ERGL:
      rgb_matrix_set_color_all(RGB_OFF);
      // ERGOL layer
      rgb_matrix_set_color(34, 0, 113, 255);
      rgb_matrix_set_color(70, 0, 113, 255);
      // Line 1
      my_set_rgb_matrix_color_line(1,213,144,0);
      // Line 2
      my_set_rgb_matrix_color_line(2,27,213,0);
      my_set_rgb_matrix_color_homerow(206,255,206);
      // Line 3
      my_set_rgb_matrix_color_line(3,0,255,167);
      // Line 4
      my_set_rgb_matrix_color_line(4,0,113,255);
      break;
    case QWER:
      rgb_matrix_set_color_all(RGB_OFF);
      // QWERTY layer
      rgb_matrix_set_color(34, 27,213,0);
      rgb_matrix_set_color(70, 27,213,0);
      // Line 1
      my_set_rgb_matrix_color_line(1,213,144,0);
      // Line 2
      my_set_rgb_matrix_color_line(2,27,213,0);
      my_set_rgb_matrix_color_homerow(206,255,206);
      // Line 3
      my_set_rgb_matrix_color_line(3,0,255,167);
      // Line 4
      my_set_rgb_matrix_color_line(4,0,113,255);
      break;
    case NUMB:
      rgb_matrix_set_color_all(RGB_OFF);
      
      rgb_matrix_set_color(0, RGB_RED);

      rgb_matrix_set_color(6, 0, 55, 255); // FX
      rgb_matrix_set_color(11, 0, 55, 255); // FX
      rgb_matrix_set_color(16, 0, 55, 255); // FX
      rgb_matrix_set_color(21, 0, 55, 255); // FX
      rgb_matrix_set_color(26, 0, 55, 255); // FX
      rgb_matrix_set_color(7, 0, 55, 255); // FX
      rgb_matrix_set_color(12, 0, 55, 255); // FX
      rgb_matrix_set_color(17, 0, 55, 255); // FX
      rgb_matrix_set_color(22, 0, 55, 255); // FX
      rgb_matrix_set_color(27, 0, 55, 255); // FX
      rgb_matrix_set_color(8, 0, 55, 255); // FX
      rgb_matrix_set_color(13, 0, 55, 255); // FX

    
      rgb_matrix_set_color(18, 0, 183, 255); // FX
      rgb_matrix_set_color(23, 0, 183, 255); // FX
      rgb_matrix_set_color(28, 0, 183, 255); // FX
      
      rgb_matrix_set_color(60, 255, 255, 255); // 0
      rgb_matrix_set_color(47, 255, 199, 0);     // number
      rgb_matrix_set_color(48, 255, 199, 0);     // number
      rgb_matrix_set_color(49, 255, 199, 0);     // number
      rgb_matrix_set_color(52, 255, 199, 0);     // number
      rgb_matrix_set_color(53, 255, 199, 0);     // number
      rgb_matrix_set_color(54, 255, 199, 0);     // number
      rgb_matrix_set_color(57, 255, 199, 0);     // number
      rgb_matrix_set_color(58, 255, 199, 0);     // number
      rgb_matrix_set_color(59, 255, 199, 0);     // number

      rgb_matrix_set_color(42, 183, 255, 0);     // calc
      rgb_matrix_set_color(43, 183, 255, 0);     // calc
      rgb_matrix_set_color(44, 183, 255, 0);     // calc
      rgb_matrix_set_color(37, 183, 255, 0);     // calc
      rgb_matrix_set_color(38, 183, 255, 0);     // calc
      rgb_matrix_set_color(39, 183, 255, 0);     // calc
      break;
    case SYMB:
      rgb_matrix_set_color_all(RGB_OFF);
      // () {} [] <>
      rgb_matrix_set_color(11, 27, 213, 0);
      rgb_matrix_set_color(16, 27, 213, 0);
      rgb_matrix_set_color(18, 27, 213, 0);
      rgb_matrix_set_color(23, 27, 213, 0);
      rgb_matrix_set_color(57, 27, 213, 0);
      rgb_matrix_set_color(58, 27, 213, 0);
      rgb_matrix_set_color(52, 27, 213, 0);
      rgb_matrix_set_color(53, 27, 213, 0);

      // +-=/*
      rgb_matrix_set_color(13, 29, 204, 67);
      rgb_matrix_set_color(12, 29, 204, 67);
      rgb_matrix_set_color(17, 29, 204, 67);
      rgb_matrix_set_color(22, 29, 204, 67);
      rgb_matrix_set_color(21, 29, 204, 67);
      rgb_matrix_set_color(1, 29, 204, 67); // anti-slash

      // `!~|&%^#
      rgb_matrix_set_color(6, 255, 199, 0);
      rgb_matrix_set_color(7, 255, 199, 0);
      rgb_matrix_set_color(8, 255, 199, 0);
      rgb_matrix_set_color(26, 255, 199, 0);
      rgb_matrix_set_color(27, 255, 199, 0);
      rgb_matrix_set_color(28, 255, 199, 0);
      rgb_matrix_set_color(62, 255, 199, 0);
      rgb_matrix_set_color(42, 255, 199, 0);
      rgb_matrix_set_color(63, 255, 199, 0);
      
      // "'
      rgb_matrix_set_color(43, 204, 202, 29);
      rgb_matrix_set_color(44, 204, 202, 29);

      // :;,.
      rgb_matrix_set_color(59, 29, 120, 204);
      rgb_matrix_set_color(54, 29, 120, 204);
      rgb_matrix_set_color(48, 29, 120, 204);
      rgb_matrix_set_color(49, 29, 120, 204);

      // @ $
      rgb_matrix_set_color(47, 174, 29, 204);
      rgb_matrix_set_color(64, 174, 29, 204);
      
      break;
    case NAVI:
      rgb_matrix_set_color_all(RGB_OFF);

      rgb_matrix_set_color(0, RGB_RED);
      rgb_matrix_set_color(36, RGB_RED);

      // Arrows
      rgb_matrix_set_color(52, 0, 113, 255);
      rgb_matrix_set_color(53, 0, 113, 255);
      rgb_matrix_set_color(58, 0, 113, 255);
      rgb_matrix_set_color(48, 0, 113, 255);
      // Home, End, PgUp, PgDown 
      rgb_matrix_set_color(57, 13, 0, 255);
      rgb_matrix_set_color(47, 13, 0, 255);
      rgb_matrix_set_color(42, 13, 0, 255);
      rgb_matrix_set_color(43, 13, 0, 255);

      // Media
      rgb_matrix_set_color(12, 27, 213, 0);
      rgb_matrix_set_color(17, 27, 213, 0);
      rgb_matrix_set_color(22, 27, 213, 0);

      // Volume
      rgb_matrix_set_color(35, 170, 0, 255);
      rgb_matrix_set_color(71, 170, 0, 255);
      rgb_matrix_set_color(64, 170, 0, 255);

      // PrintScreen
      rgb_matrix_set_color(62, 255, 255, 255);
      break;
    case MOUS:
      rgb_matrix_set_color_all(RGB_OFF);
      // mouse movements
      rgb_matrix_set_color(52, 0, 113, 255);
      rgb_matrix_set_color(53, 0, 113, 255);
      rgb_matrix_set_color(58, 0, 113, 255);
      rgb_matrix_set_color(48, 0, 113, 255);
      // wheel

      rgb_matrix_set_color(7, 13, 0, 255);
      rgb_matrix_set_color(12, 13, 0, 255);
      rgb_matrix_set_color(17, 13, 0, 255);
      rgb_matrix_set_color(22, 13, 0, 255);
      // clicks
      rgb_matrix_set_color(32, 170, 0, 255);
      rgb_matrix_set_color(33, 170, 0, 255);
      rgb_matrix_set_color(68, 170, 0, 255);
      rgb_matrix_set_color(69, 170, 0, 255);
      break;
    default:
      rgb_matrix_set_color_all(RGB_OFF);
      break;
    }
  }
  return false;
}

void caps_word_set_user(bool active) {
  if (active) {
    rgb_matrix_set_color(40, 255, 255, 255);
  } else {
    rgb_matrix_set_color(40, RGB_OFF);
  }
}
