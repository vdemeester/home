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
    BEPO,
    ERGL,
    QWER,
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

#define COMBO_REF_DEFAULT QWER

uint8_t combo_ref_from_layer(uint8_t layer){
    switch (get_highest_layer(layer_state)){
        case BEPO: return QWER;
        default: return QWER;
    }
    return layer;  // important if default is not in case.
}

const uint16_t PROGMEM combo_to_bepo[] = {LT(NAVI,KC_BSPC), SFTLLCK, COMBO_END};
const uint16_t PROGMEM combo_to_ergol[] = {LT(NUMB,KC_SPC), LT(SYMB, KC_ENT), COMBO_END};
const uint16_t PROGMEM combo_to_qwerty[] = {KC_DEL, KC_RALT, COMBO_END};
const uint16_t PROGMEM combo_qwe_escape[] = {ALT_L, GUI_SCLN, COMBO_END};
const uint16_t PROGMEM combo_toggle_mouse[] = {KC_Q, KC_R, COMBO_END};

combo_t key_combos[] = {
  // Layers
  COMBO(combo_to_bepo, TO(BEPO)),
  COMBO(combo_to_ergol, TO(ERGL)),
  COMBO(combo_to_qwerty, TO(QWER)),
  COMBO(combo_toggle_mouse, TG(MOUS)),
  // Others
  COMBO(combo_qwe_escape, KC_ESC),
};

tap_dance_action_t tap_dance_actions[] = {
};

// clang-format off
const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
    [BEPO] = LAYOUT(
        KC_EQL,  KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    XXXXXXX,           XXXXXXX, KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_MINS,
        KC_TAB,  KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    XXXXXXX,           XXXXXXX, KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC,
        // KC_TAB,  KC_B,    KC_É,    KC_P,    KC_O,    KC_È,    XXXXXXX,           XXXXXXX, caret,    KC_V,    KC_D,    KC_L,    KC_J,    KC_Z,
        KC_EQL,  GUI_A,    ALT_S,    SFT_D,    CTL_F,    HYP_G,    XXXXXXX,           XXXXXXX, HYP_H,    CTL_J,    SFT_K,    ALT_L,    GUI_SCLN, KC_QUOT,
        // KC_EQL,  KC_A,    KC_U,    KC_I,    KC_E,    KC_COM,  XXXXXXX,           XXXXXXX,  KC_H,    KC_J,    KC_K,    KC_L,    LT(NAVI, KC_SCLN), LGUI_T(KC_QUOT),
        KC_GRV,  KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,                                KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH, KC_RBRC,
        //KC_GRV, LCTL_T(KC_Z),KC_X,KC_C,    KC_V,    KC_B,                                KC_N,    KC_M,    KC_COMM, KC_DOT,  RCTL_T(KC_SLSH), KC_RSFT,
        XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX, KC_DEL,  _______,               _______,      KC_RALT, XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX,
	LT(NUMB, KC_SPC),  LT(NAVI,KC_BSPC), XXXXXXX,           XXXXXXX,  SFTLLCK,  LT(SYMB, KC_ENT)
    ),
    [ERGL] = LAYOUT(
        KC_EQL,  KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    XXXXXXX,           XXXXXXX, KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_MINS,
        KC_TAB,  KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    XXXXXXX,           XXXXXXX, KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC,
        KC_EQL,  GUI_A,    ALT_S,    SFT_D,    CTL_F,    HYP_G,    XXXXXXX,           XXXXXXX, HYP_H,    CTL_J,    SFT_K,    ALT_L,    GUI_SCLN, KC_QUOT,
        KC_GRV,  KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,                                KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH, KC_RBRC,
        XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX, KC_DEL,  _______,               _______,      KC_RALT, XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX,
	LT(NUMB, KC_SPC),  LT(NAVI,KC_BSPC), XXXXXXX,           XXXXXXX,  SFTLLCK,  LT(SYMB, KC_ENT)
    ),
    [QWER] = LAYOUT(
        KC_EQL,  KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    XXXXXXX,           XXXXXXX, KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_MINS,
        KC_TAB,  KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    XXXXXXX,           XXXXXXX, KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC,
        KC_EQL,  GUI_A,    ALT_S,    SFT_D,    CTL_F,    HYP_G,    XXXXXXX,           XXXXXXX, HYP_H,    CTL_J,    SFT_K,    ALT_L,    GUI_SCLN, KC_QUOT,
        KC_GRV,  KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,                                KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH, KC_RBRC,
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
      rgb_matrix_set_color(11, 255, 199, 0);
      rgb_matrix_set_color(16, 255, 199, 0);
      rgb_matrix_set_color(18, 255, 199, 0);
      rgb_matrix_set_color(23, 255, 199, 0);
      rgb_matrix_set_color(57, 255, 199, 0);
      rgb_matrix_set_color(58, 255, 199, 0);
      rgb_matrix_set_color(52, 255, 199, 0);
      rgb_matrix_set_color(53, 255, 199, 0);

      // +-=/*
      // 29 204 67

      // `!~|&%^#
      // 213, 144, 0

      // "'
      // 204, 202, 29

      // :;,.
      // 29, 120, 204

      // @
      // 174, 29, 204
      
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
      break;
    default:
      rgb_matrix_set_color_all(RGB_OFF);
      break;
    }
  }
  return false;
}
