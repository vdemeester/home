#include QMK_KEYBOARD_H
#include "moonlander_keymap.h"

extern rgb_config_t rgb_matrix_config;

void my_set_rgb_matrix_color_range(int start, int end, uint8_t red, uint8_t green, uint8_t blue) {
    for (int base = start; base < end; base += 5) {
        for (int offset = 0; offset < 3; offset++) {
            int i = base + offset;
            if (i < end) {
                rgb_matrix_set_color(i, red, green, blue);
            }
        }
    }
}

void set_a_z_color(uint8_t red, uint8_t green, uint8_t blue) {
    my_set_rgb_matrix_color_range(6, 29, red, green, blue);
    my_set_rgb_matrix_color_range(42, 65, red, green, blue);
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
//    │ 4  │ 9  │ 14 │ 19 │ 24 │    │   32    │   │   68    │    │ 60 │ 55 │ 50 │ 45 │ 40 │
//    └────┴────┴────┴────┴────┼────┼────┬────┤   ├────┬────┼────┼────┴────┴────┴────┴────┘
//                             │ 33 │ 34 │ 35 │   │ 69 │ 70 │ 71 │
//                             └────┴────┴────┘   └────┴────┴────┘
// Runs constantly in the background, in a loop.
bool rgb_matrix_indicators_user(void) {
    if (rgb_matrix_get_flags() & (LED_FLAG_KEYLIGHT | LED_FLAG_MODIFIER)) {
        switch (get_highest_layer(layer_state)) {
            case MDIA:
                rgb_matrix_set_color_all(RGB_RED);
                rgb_matrix_set_color(38, RGB_BLUE); // screenshot key
                break;
            case BASE:
                rgb_matrix_set_color_all(RGB_OFF);
                // set_a_z_color(RGB_WHITE);
                rgb_matrix_set_color(22, RGB_WHITE); // F
                rgb_matrix_set_color(58, RGB_WHITE); // J
                // rgb_matrix_set_color(29, RGB_MAGENTA);
                // rgb_matrix_set_color(30, RGB_MAGENTA);
                // rgb_matrix_set_color(31, RGB_MAGENTA);
                // rgb_matrix_set_color(65, RGB_MAGENTA);
                // rgb_matrix_set_color(66, RGB_MAGENTA);
                // rgb_matrix_set_color(67, RGB_MAGENTA);
                // rgb_matrix_set_color(60, RGB_YELLOW);
                // rgb_matrix_set_color(24, RGB_YELLOW);
                // rgb_matrix_set_color(2, RGB_CHARTREUSE);
                // rgb_matrix_set_color(38, RGB_CHARTREUSE);
                break;
            case SYMB:
                rgb_matrix_set_color_all(RGB_YELLOW);
                rgb_matrix_set_color(60, RGB_MAGENTA); // 0
                rgb_matrix_set_color(47, RGB_RED);     // number
                rgb_matrix_set_color(48, RGB_RED);     // number
                rgb_matrix_set_color(49, RGB_RED);     // number
                rgb_matrix_set_color(52, RGB_RED);     // number
                rgb_matrix_set_color(53, RGB_RED);     // number
                rgb_matrix_set_color(54, RGB_RED);     // number
                rgb_matrix_set_color(57, RGB_RED);     // number
                rgb_matrix_set_color(58, RGB_RED);     // number
                rgb_matrix_set_color(59, RGB_RED);     // number
                break;
            case ACCENT:
                rgb_matrix_set_color_all(RGB_CYAN);
                rgb_matrix_set_color(7, RGB_RED);
                rgb_matrix_set_color(62, RGB_RED);
                rgb_matrix_set_color(57, RGB_RED);
                rgb_matrix_set_color(52, RGB_RED);
                rgb_matrix_set_color(47, RGB_RED);
                rgb_matrix_set_color(26, RGB_RED);
                rgb_matrix_set_color(21, RGB_RED);
                rgb_matrix_set_color(18, RGB_RED);
                rgb_matrix_set_color(16, RGB_RED);
                rgb_matrix_set_color(15, RGB_RED);
                rgb_matrix_set_color(12, RGB_RED);
                rgb_matrix_set_color(11, RGB_RED);
                break;
            case GAME:
                rgb_matrix_set_color_all(RGB_WHITE);
                rgb_matrix_set_color(11, RGB_RED); //
                rgb_matrix_set_color(12, RGB_RED); //
                rgb_matrix_set_color(7, RGB_RED);  //
                rgb_matrix_set_color(17, RGB_RED); //
                break;
            case STUFF:
                rgb_matrix_set_color_all(RGB_MAGENTA);
                rgb_matrix_set_color(11, RGB_RED); //
                rgb_matrix_set_color(12, RGB_RED); //
                rgb_matrix_set_color(7, RGB_RED);  //
                break;
#if 0
            case NUMB:
                rgb_matrix_set_color_all(RGB_WHITE);
                rgb_matrix_set_color(55, RGB_MAGENTA); // 0
                rgb_matrix_set_color(47, RGB_YELLOW);  // number
                rgb_matrix_set_color(48, RGB_YELLOW);  // number
                rgb_matrix_set_color(49, RGB_YELLOW);  // number
                rgb_matrix_set_color(52, RGB_YELLOW);  // number
                rgb_matrix_set_color(53, RGB_YELLOW);  // number
                rgb_matrix_set_color(54, RGB_YELLOW);  // number
                rgb_matrix_set_color(57, RGB_YELLOW);  // number
                rgb_matrix_set_color(58, RGB_YELLOW);  // number
                rgb_matrix_set_color(59, RGB_YELLOW);  // number

                rgb_matrix_set_color(19, RGB_MAGENTA); // 0
                rgb_matrix_set_color(11, RGB_YELLOW);  // number
                rgb_matrix_set_color(12, RGB_YELLOW);  // number
                rgb_matrix_set_color(13, RGB_YELLOW);  // number
                rgb_matrix_set_color(16, RGB_YELLOW);  // number
                rgb_matrix_set_color(17, RGB_YELLOW);  // number
                rgb_matrix_set_color(18, RGB_YELLOW);  // number
                rgb_matrix_set_color(21, RGB_YELLOW);  // number
                rgb_matrix_set_color(22, RGB_YELLOW);  // number
                rgb_matrix_set_color(23, RGB_YELLOW);  // number
                break;
#endif
            default:
                break;
        }
    }
    return false;
}
