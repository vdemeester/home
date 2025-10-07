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
#include "version.h"
#include "keymap_us.h"
#include "keymap_bepo.h"

enum layers {
    QWER,
    BEPO,
    ERGL,
    SYMB,
    NUMB,
    NAVI,
    MOUS,
};

enum custom_keycodes {
    VRSN = SAFE_RANGE,
};

const key_override_t *key_overrides[] = {
};

combo_t key_combos[] = {
};

tap_dance_action_t tap_dance_actions[] = {
};

// clang-format off
const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
    [QWER] = LAYOUT(
        KC_EQL,  KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    XXXXXXX,           XXXXXXX, KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_MINS,
        KC_TAB,  KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    XXXXXXX,           XXXXXXX, KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC,
        KC_EQL,  GUI_A,    ALT_S,    SFT_D,    CTL_F,    HYP_G,    XXXXXXX,           XXXXXXX, HYP_H,    CTL_J,    SFT_K,    ALT_L,    GUI_SCLN, KC_QUOT,
        KC_GRV,  KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,                                KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH, KC_RBRC,
        XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX, KC_DEL,  _______,               _______,      KC_RALT, XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX,
	LT(NUMB, KC_SPC),  LT(NAVI,KC_BSPC), XXXXXXX,           XXXXXXX,  SFTLLCK,  LT(SYMB, KC_ENT)
    ),
    [BEPO] = LAYOUT(
        KC_EQL,  KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    XXXXXXX,           XXXXXXX, KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_MINS,
        KC_TAB,  KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    XXXXXXX,           XXXXXXX, KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC,
        // KC_TAB,  KC_B,    KC_É,    KC_P,    KC_O,    KC_È,    XXXXXXX,           XXXXXXX, caret,    KC_V,    KC_D,    KC_L,    KC_J,    KC_Z,
        KC_EQL,  KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    XXXXXXX,           XXXXXXX,  KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, KC_QUOT,
        // KC_EQL,  KC_A,    KC_U,    KC_I,    KC_E,    KC_COM,  XXXXXXX,           XXXXXXX,  KC_H,    KC_J,    KC_K,    KC_L,    LT(NAVI, KC_SCLN), LGUI_T(KC_QUOT),
        KC_GRV,  KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,                                KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH, KC_RBRC,
        //KC_GRV, LCTL_T(KC_Z),KC_X,KC_C,    KC_V,    KC_B,                                KC_N,    KC_M,    KC_COMM, KC_DOT,  RCTL_T(KC_SLSH), KC_RSFT,
        XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX, KC_DEL,  _______,               _______,      KC_RALT, XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX,
	LT(NUMB, KC_SPC),  LT(NAVI,KC_BSPC), XXXXXXX,           XXXXXXX,  SFTLLCK,  LT(SYMB, KC_ENT)
    ),
    [ERGL] = LAYOUT(
        KC_EQL,  KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    XXXXXXX,           XXXXXXX, KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_MINS,
        KC_DEL,  KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    XXXXXXX,           XXXXXXX, KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_BSLS,
        KC_BSPC, KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    XXXXXXX,           XXXXXXX,  KC_H,    KC_J,    KC_K,    KC_L,    LT(NAVI, KC_SCLN), LGUI_T(KC_QUOT),
        KC_LSFT, LCTL_T(KC_Z),KC_X,KC_C,    KC_V,    KC_B,                                KC_N,    KC_M,    KC_COMM, KC_DOT,  RCTL_T(KC_SLSH), KC_RSFT,
        XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX, KC_DEL,  _______,               _______,      KC_RALT, XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX,
	LT(NUMB, KC_SPC),  LT(NAVI,KC_BSPC), XXXXXXX,           XXXXXXX,  SFTLLCK,  LT(SYMB, KC_ENT)
    ),

    [SYMB] = LAYOUT(
        VRSN,    _______,   _______,   _______,   _______,   _______,   _______,           _______, _______,  _______,  _______,  _______, _______,  _______,
        KC_BSLS, KC_EXLM, KC_AT,   KC_LCBR, KC_RCBR, KC_PIPE, _______,           _______, KC_UP,   KC_7,    KC_8,    KC_9,    KC_ASTR, KC_F12,
        _______, KC_EXLM, KC_DLR,  KC_LPRN, KC_RPRN, KC_GRV,  _______,           _______, KC_DOWN, KC_4,    KC_5,    KC_6,    KC_PLUS, _______,
        _______, KC_TILD, KC_CIRC, KC_LBRC, KC_RBRC, KC_TILD,                             KC_AMPR, KC_1,    KC_2,    KC_3,    KC_BSLS, _______,
        EE_CLR,  _______, _______, _______, _______,          RM_VALU,           RM_TOGG,          _______, KC_DOT,  KC_0,    KC_EQL,  _______,
                                            RM_HUED, RM_VALD, RM_HUEU, TOGGLE_LAYER_COLOR,_______, _______
    ),
    [NUMB] = LAYOUT(
        VRSN,    _______,   _______,   _______,   _______,   _______,   _______,           _______, _______,  _______,  _______,  _______, _______,  _______,
        _______, KC_F1,   KC_F2,   KC_F3,   KC_F4,   KC_F5,   _______,           _______, _______, KC_7,    KC_8,    KC_9,    KC_PAST, KC_PSLS,
        _______, KC_F6,   KC_F7,   KC_F8,   KC_F9,   KC_F10,  _______,           _______, _______, KC_4,    KC_5,    KC_6,    KC_PMNS, KC_PPLS,
        _______, KC_F11,  KC_F12,  KC_F13,  KC_F14,  KC_F15,                              _______, KC_1,    KC_2,    KC_3,    KC_PDOT, KC_PCMM,
        EE_CLR,  _______, _______, _______, _______,          _______,           _______,          _______, _______, _______, KC_0,    _______,
                                            _______, _______, _______,           _______, _______, _______
    ),
    [NAVI] = LAYOUT(
        VRSN,    _______,   _______,   _______,   _______,   _______,   _______,           _______, _______,  _______,  _______,  _______, _______,  QK_BOOT,
        _______, _______, KC_AT,   KC_LCBR, KC_RCBR, KC_PIPE,  _______,           _______, KC_PSCR,   KC_HOME,    KC_UP,    KC_END,    KC_PGUP, _______,
        _______, KC_HASH, KC_MPRV, KC_MPLY, KC_MNXT, _______,  _______,           _______, _______, KC_LEFT,    KC_DOWN,    KC_RGHT,    KC_PGDN, _______,
        _______, _______, _______, _______, _______, _______,                              KC_MUTE, _______, _______, _______, _______, _______,
        EE_CLR,  _______, _______, _______, _______,          KC_VOLD,           KC_VOLU,           _______, _______, _______, _______,  _______,
                                            _______, _______, _______,           _______, _______, _______
    ),

    [MOUS] = LAYOUT(
        LED_LEVEL,_______,_______, _______, _______, _______, _______,           _______, _______, _______, _______, _______, _______, QK_BOOT,
        _______, _______, _______, _______,   _______, _______, _______,           _______, _______, _______, MS_UP, _______, _______, _______,
        _______, MS_WHLL, MS_WHLU, MS_WHLD, MS_WHLR, _______, _______,           _______, _______, MS_LEFT, MS_DOWN, MS_RGHT, _______, _______,
        _______, _______, _______, _______, _______, _______,                              _______, _______, _______, _______, _______, _______,
        EE_CLR,  _______, _______, _______, _______,          KC_VOLD,           KC_VOLU,           _______, _______, _______, _______,  _______,
                                            _______, MS_BTN1, MS_BTN3,           MS_BTN3, MS_BTN2, _______
    ),
};

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
    if (record->event.pressed) {
        switch (keycode) {
        case VRSN:
            SEND_STRING (QMK_KEYBOARD "/" QMK_KEYMAP " @ " QMK_VERSION);
            return false;
        case SFTLLCK:
            if (record->tap.count) {
                if (record->event.pressed) {
                    // Toggle the lock on the highest layer.
                    layer_lock_invert(get_highest_layer(layer_state));
                }
                return false;
            }
	    break;
	}
    }
    return true;
}

extern rgb_config_t rgb_matrix_config;

// Define common colors as RGB values
#define RGB_OFF 0, 0, 0
#define RGB_ORANGE_YELLOW 213, 144, 0
#define RGB_GREEN 27, 213, 0
#define RGB_GREEN_LIGHT 206, 255, 206
#define RGB_CYAN 0, 255, 167
#define RGB_BLUE_LIGHT 0, 113, 255
#define RGB_BLUE 0, 55, 255
#define RGB_BLUE_BRIGHT 0, 183, 255
#define RGB_WHITE 255, 255, 255
#define RGB_YELLOW 255, 199, 0
#define RGB_YELLOW_GREEN 183, 255, 0
#define RGB_PURPLE 13, 0, 255
#define RGB_GREEN_MEDIA 27, 213, 0
#define RGB_PURPLE_VOL 170, 0, 255

// LED map for each layer: [layer][led_index] = {r, g, b}
// 72 LEDs total (0-71) as shown in the matrix diagram below
const uint8_t PROGMEM ledmap[][72][3] = {
    // QWER layer
    [QWER] = {
        // Row 0: 0, 5, 10, 15, 20, 25, 29 | 65, 61, 56, 51, 46, 41, 36
        [0] = {RGB_OFF}, [5] = {RGB_ORANGE_YELLOW}, [10] = {RGB_ORANGE_YELLOW}, [15] = {RGB_ORANGE_YELLOW}, [20] = {RGB_ORANGE_YELLOW}, [25] = {RGB_ORANGE_YELLOW}, [29] = {RGB_OFF},
        [65] = {RGB_OFF}, [61] = {RGB_ORANGE_YELLOW}, [56] = {RGB_ORANGE_YELLOW}, [51] = {RGB_ORANGE_YELLOW}, [46] = {RGB_ORANGE_YELLOW}, [41] = {RGB_ORANGE_YELLOW}, [36] = {RGB_ORANGE_YELLOW},
        // Row 1: 1, 6, 11, 16, 21, 26, 30 | 66, 62, 57, 52, 47, 42, 37
        [1] = {RGB_ORANGE_YELLOW}, [6] = {RGB_ORANGE_YELLOW}, [11] = {RGB_ORANGE_YELLOW}, [16] = {RGB_ORANGE_YELLOW}, [21] = {RGB_ORANGE_YELLOW}, [26] = {RGB_ORANGE_YELLOW}, [30] = {RGB_OFF},
        [66] = {RGB_OFF}, [62] = {RGB_ORANGE_YELLOW}, [57] = {RGB_ORANGE_YELLOW}, [52] = {RGB_ORANGE_YELLOW}, [47] = {RGB_ORANGE_YELLOW}, [42] = {RGB_ORANGE_YELLOW}, [37] = {RGB_ORANGE_YELLOW},
        // Row 2: 2, 7, 12, 17, 22, 27, 31 | 67, 63, 58, 53, 48, 43, 38 (home row)
        [2] = {RGB_GREEN}, [7] = {RGB_GREEN_LIGHT}, [12] = {RGB_GREEN_LIGHT}, [17] = {RGB_GREEN_LIGHT}, [22] = {RGB_GREEN_LIGHT}, [27] = {RGB_GREEN}, [31] = {RGB_OFF},
        [67] = {RGB_OFF}, [63] = {RGB_GREEN}, [58] = {RGB_GREEN_LIGHT}, [53] = {RGB_GREEN_LIGHT}, [48] = {RGB_GREEN_LIGHT}, [43] = {RGB_GREEN_LIGHT}, [38] = {RGB_GREEN},
        // Row 3: 3, 8, 13, 18, 23, 28 | 64, 59, 54, 49, 44, 39
        [3] = {RGB_CYAN}, [8] = {RGB_CYAN}, [13] = {RGB_CYAN}, [18] = {RGB_CYAN}, [23] = {RGB_CYAN}, [28] = {RGB_CYAN},
        [64] = {RGB_CYAN}, [59] = {RGB_CYAN}, [54] = {RGB_CYAN}, [49] = {RGB_CYAN}, [44] = {RGB_CYAN}, [39] = {RGB_CYAN},
        // Row 4: 4, 9, 14, 19, 24, 32, 33, 34, 35 | 60, 55, 50, 45, 40, 68, 69, 70, 71
        [4] = {RGB_OFF}, [9] = {RGB_OFF}, [14] = {RGB_OFF}, [19] = {RGB_OFF}, [24] = {RGB_BLUE_LIGHT},
        [32] = {RGB_BLUE_LIGHT}, [33] = {RGB_BLUE_LIGHT}, [34] = {RGB_OFF}, [35] = {RGB_BLUE_LIGHT},
        [60] = {RGB_BLUE_LIGHT}, [55] = {RGB_OFF}, [50] = {RGB_OFF}, [45] = {RGB_OFF}, [40] = {RGB_OFF},
        [68] = {RGB_BLUE_LIGHT}, [69] = {RGB_BLUE_LIGHT}, [70] = {RGB_OFF}, [71] = {RGB_BLUE_LIGHT},
    },
    // NUMB layer
    [NUMB] = {
        // Reset all keys
        [0] = {RGB_RED},
        // F keys - blue
        [6] = {RGB_BLUE}, [11] = {RGB_BLUE}, [16] = {RGB_BLUE}, [21] = {RGB_BLUE}, [26] = {RGB_BLUE},
        [7] = {RGB_BLUE}, [12] = {RGB_BLUE}, [17] = {RGB_BLUE}, [22] = {RGB_BLUE}, [27] = {RGB_BLUE},
        [8] = {RGB_BLUE}, [13] = {RGB_BLUE},
        // More F keys - bright blue
        [18] = {RGB_BLUE_BRIGHT}, [23] = {RGB_BLUE_BRIGHT}, [28] = {RGB_BLUE_BRIGHT},
        // Numbers - yellow
        [47] = {RGB_YELLOW}, [48] = {RGB_YELLOW}, [49] = {RGB_YELLOW},
        [52] = {RGB_YELLOW}, [53] = {RGB_YELLOW}, [54] = {RGB_YELLOW},
        [57] = {RGB_YELLOW}, [58] = {RGB_YELLOW}, [59] = {RGB_YELLOW},
        [60] = {RGB_WHITE}, // 0
        // Calc operators - yellow-green
        [42] = {RGB_YELLOW_GREEN}, [43] = {RGB_YELLOW_GREEN}, [44] = {RGB_YELLOW_GREEN},
        [37] = {RGB_YELLOW_GREEN}, [38] = {RGB_YELLOW_GREEN}, [39] = {RGB_YELLOW_GREEN},
    },
    // SYMB layer
    [SYMB] = {
        // Brackets - yellow
        [11] = {RGB_YELLOW}, [16] = {RGB_YELLOW}, [18] = {RGB_YELLOW}, [23] = {RGB_YELLOW},
        [57] = {RGB_YELLOW}, [58] = {RGB_YELLOW}, [52] = {RGB_YELLOW}, [53] = {RGB_YELLOW},
    },
    // NAVI layer
    [NAVI] = {
        [0] = {RGB_RED}, [36] = {RGB_RED},
        // Arrows - blue light
        [52] = {RGB_BLUE_LIGHT}, [53] = {RGB_BLUE_LIGHT}, [58] = {RGB_BLUE_LIGHT}, [48] = {RGB_BLUE_LIGHT},
        // Home, End, PgUp, PgDown - purple
        [57] = {RGB_PURPLE}, [47] = {RGB_PURPLE}, [42] = {RGB_PURPLE}, [43] = {RGB_PURPLE},
        // Media - green
        [12] = {RGB_GREEN_MEDIA}, [17] = {RGB_GREEN_MEDIA}, [22] = {RGB_GREEN_MEDIA},
        // Volume - purple
        [35] = {RGB_PURPLE_VOL}, [71] = {RGB_PURPLE_VOL}, [64] = {RGB_PURPLE_VOL},
        // PrintScreen - white
        [62] = {RGB_WHITE},
    },
};

void set_layer_color(int layer) {
    for (int i = 0; i < 72; i++) {
        uint8_t r = pgm_read_byte(&ledmap[layer][i][0]);
        uint8_t g = pgm_read_byte(&ledmap[layer][i][1]);
        uint8_t b = pgm_read_byte(&ledmap[layer][i][2]);
        rgb_matrix_set_color(i, r, g, b);
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
//                             │ 32 │ 33 │ 34 │   │ 68 │ 69 │ 70 │
//                             └────┴────┴────┘   └────┴────┴────┘
// Runs constantly in the background, in a loop.
bool rgb_matrix_indicators_user(void) {
  if (rgb_matrix_get_flags() & (LED_FLAG_KEYLIGHT | LED_FLAG_MODIFIER)) {
    uint8_t layer = get_highest_layer(layer_state);
    switch (layer) {
    case QWER:
    case NUMB:
    case SYMB:
    case NAVI:
      rgb_matrix_set_color_all(RGB_OFF);
      set_layer_color(layer);
      break;
    case MOUS:
      // MOUS layer has no custom LED configuration
      break;
    default:
      rgb_matrix_set_color_all(RGB_OFF);
      break;
    }
  }
  return false;
}
